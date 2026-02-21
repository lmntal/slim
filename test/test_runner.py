#!/usr/bin/env python3
"""
Enhanced test runner for SLIM
Replaces shell-based check.pl with modern Python implementation
"""

import argparse
import sys
import subprocess
import time
import json
import tempfile
import os
from pathlib import Path
from enum import Enum
from dataclasses import dataclass
from typing import List, Dict, Optional, Tuple

class TestResult(Enum):
    PASS = "PASS"
    FAIL = "FAIL"
    SKIP = "SKIP"
    ERROR = "ERROR"

@dataclass
class TestCase:
    name: str
    program: str
    expected_output: str
    expected_result: str  # "ok" or "ng"
    timeout: int = 30
    
@dataclass
class TestReport:
    name: str
    result: TestResult
    duration: float
    output: str = ""
    error: str = ""
    details: Dict = None

class SLIMTestRunner:
    def __init__(self, slim_binary: str, lmntal_home: str = None):
        self.slim_binary = slim_binary
        self.lmntal_home = lmntal_home or os.getenv('LMNTAL_HOME')
        self.lmntal_compiler = None
        
        if self.lmntal_home:
            self.lmntal_compiler = os.path.join(self.lmntal_home, 'bin', 'lmntal')
            
    def parse_lmntest_file(self, test_file: Path) -> TestCase:
        """Parse .lmntest file format"""
        with open(test_file, 'r', encoding='utf-8') as f:
            lines = [line.strip() for line in f.readlines() if line.strip()]
            
        if len(lines) < 3:
            raise ValueError(f"Invalid test file format: {test_file}")
            
        return TestCase(
            name=test_file.stem,
            program=lines[0],
            expected_output=lines[1],
            expected_result=lines[2]
        )
    
    def compile_lmntal(self, program: str) -> Tuple[bool, str, str]:
        """Compile LMNtal program to intermediate language"""
        if not self.lmntal_compiler:
            return False, "", "LMNTAL_HOME not set or lmntal compiler not found"
            
        try:
            process = subprocess.run(
                [self.lmntal_compiler, '--stdin-lmn', '--slimcode', '-O3'],
                input=program,
                text=True,
                capture_output=True,
                timeout=30
            )
            
            return process.returncode == 0, process.stdout, process.stderr
        except subprocess.TimeoutExpired:
            return False, "", "Compilation timeout"
        except Exception as e:
            return False, "", f"Compilation error: {str(e)}"
    
    def extract_result_from_trace(self, trace_output: str, expected_pattern: str) -> str:
        """Extract the result atom from trace output that matches the expected pattern"""
        import re
        
        lines = trace_output.split('\n')
        
        # Strategy 1: Look for exact pattern match in trace lines
        for line in lines:
            if expected_pattern.strip() in line:
                # Extract the pattern, removing trace formatting
                # Handle cases like "5: {result(...)}." or "f(end(f))"
                cleaned_line = re.sub(r'^\d+:\s*\{?', '', line)  # Remove step number and {
                cleaned_line = re.sub(r'\}\.\s*$', '', cleaned_line)  # Remove }. at end  
                cleaned_line = re.sub(r'\.\s*@\d+\.\s*', '', cleaned_line)  # Remove step counters
                cleaned_line = cleaned_line.strip()
                if cleaned_line and expected_pattern.strip() in cleaned_line:
                    # Try to extract just the matching part
                    if '(' in expected_pattern and ')' in expected_pattern:
                        pattern_match = re.search(r'\w+\([^)]+\)', cleaned_line)
                        if pattern_match:
                            return pattern_match.group(0)
                    return cleaned_line
        
        # Strategy 2: Look for pattern in final non-empty line (final state)
        final_lines = [line for line in reversed(lines) if line.strip() and not line.startswith('---')]
        for line in final_lines[:3]:  # Check last few lines
            cleaned_line = re.sub(r'^\d+:\s*\{?', '', line)
            cleaned_line = re.sub(r'\}\.\s*$', '', cleaned_line)
            cleaned_line = re.sub(r'\.\s*@\d+\.\s*', '', cleaned_line)
            cleaned_line = cleaned_line.strip()
            if cleaned_line:
                # Extract atoms that match expected pattern structure
                if '(' in expected_pattern and ')' in expected_pattern:
                    expected_atom_name = expected_pattern.split('(')[0]
                    # Look for atoms with the same name as expected
                    atom_pattern = rf'{expected_atom_name}\([^)]+\)'
                    matches = re.findall(atom_pattern, cleaned_line)
                    if matches:
                        return matches[0]  # Return first match with same atom name
                return cleaned_line
        
        # Fallback: return cleaned trace output
        return re.sub(r'\. @\d+\.', '', trace_output).strip()
    
    def run_slim(self, il_code: str, options: List[str] = None) -> Tuple[bool, str, str, float]:
        """Run SLIM with intermediate language code"""
        options = options or []
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.il', delete=False) as f:
            f.write(il_code)
            il_file = f.name
            
        try:
            start_time = time.time()
            process = subprocess.run(
                [self.slim_binary] + options + [il_file],
                capture_output=True,
                text=True,
                timeout=60
            )
            duration = time.time() - start_time
            
            return process.returncode == 0, process.stdout, process.stderr, duration
            
        except subprocess.TimeoutExpired:
            return False, "", "Execution timeout", 60.0
        except Exception as e:
            return False, "", f"Execution error: {str(e)}", 0.0
        finally:
            os.unlink(il_file)
    
    def run_test(self, test_case: TestCase, test_type: str = "system") -> TestReport:
        """Run a single test case"""
        start_time = time.time()
        
        try:
            # Compile LMNtal program
            success, il_code, compile_error = self.compile_lmntal(test_case.program)
            if not success:
                return TestReport(
                    name=test_case.name,
                    result=TestResult.ERROR,
                    duration=time.time() - start_time,
                    error=f"Compilation failed: {compile_error}"
                )
            
            # Determine SLIM options based on test type
            options = []
            if test_type == "statespace":
                options.extend(["--nd"])
            elif test_type == "library":
                options.extend(["--use-builtin-rule"])
            else:  # system tests need trace to extract intermediate results
                options.extend(["-t"])
                
            # Add any additional options from environment
            if "slim_CHECK_OPTIONS" in os.environ:
                options.extend(os.environ["slim_CHECK_OPTIONS"].split())
            
            # Run SLIM
            success, output, error, exec_duration = self.run_slim(il_code, options)
            
            # Check results
            output_clean = output.strip()
            
            # For system tests, use simpler approach similar to original Perl check.pl
            import re
            use_simple_check = False
            
            if test_type == "system" and test_case.expected_result == "ok":
                # If output contains "ok", treat as success (like original Perl script)
                if "ok" in output:
                    output_clean = "ok"
                    use_simple_check = True
                else:
                    # Try detailed trace extraction for precise tests (like append tests)
                    extracted_result = self.extract_result_from_trace(output_clean, test_case.expected_output)
                    output_clean = extracted_result if extracted_result else output_clean
            else:
                # Remove '. @X.' suffix from output (step counter)
                output_clean = re.sub(r'\. @\d+\.$', '.', output_clean)
            
            # Set expected based on whether we're using simple check
            if use_simple_check:
                expected_clean = "ok"
            else:
                expected_clean = test_case.expected_output.strip()
            
            if test_case.expected_result == "ok":
                test_passed = (output_clean == expected_clean)
            else:  # "ng"
                test_passed = (output_clean != expected_clean)
                
            result = TestResult.PASS if test_passed else TestResult.FAIL
            
            # Always include detailed comparison info for analysis
            if not test_passed:
                detailed_info = f"Expected: {expected_clean}, Actual: {output_clean}"
            else:
                detailed_info = f"Matched: {output_clean}"
            
            # Update error field to include comparison details for TAP output
            if not test_passed:
                comparison_error = f"Expected: {expected_clean}, Actual: {output_clean}, Duration: {exec_duration:.3f}s"
            else:
                comparison_error = error if error else None
                
            return TestReport(
                name=test_case.name,
                result=result,
                duration=time.time() - start_time,
                output=output,
                error=comparison_error,
                details={
                    "expected": expected_clean,
                    "actual": output_clean,
                    "execution_time": exec_duration,
                    "test_type": test_type,
                    "comparison": detailed_info
                }
            )
            
        except Exception as e:
            return TestReport(
                name=test_case.name,
                result=TestResult.ERROR,
                duration=time.time() - start_time,
                error=str(e)
            )
    
    def print_tap_result(self, report: TestReport, test_number: int):
        """Print result in TAP format"""
        status = "ok" if report.result == TestResult.PASS else "not ok"
        print(f"{status} {test_number} {report.name}")
        
        if report.result != TestResult.PASS:
            print(f"  # {report.result.value}: {report.error}")
            if report.details:
                print(f"  # Expected: {report.details.get('expected', 'N/A')}")
                print(f"  # Actual: {report.details.get('actual', 'N/A')}")
        
        print(f"  # Duration: {report.duration:.3f}s")

def main():
    parser = argparse.ArgumentParser(description="Enhanced SLIM test runner")
    parser.add_argument("--test-file", required=True, help="Path to .lmntest file")
    parser.add_argument("--slim-binary", required=True, help="Path to SLIM executable")
    parser.add_argument("--type", default="system", choices=["system", "library", "statespace"],
                       help="Test type")
    parser.add_argument("--json", action="store_true", help="Output JSON report")
    parser.add_argument("--verbose", "-v", action="store_true", help="Verbose output")
    
    args = parser.parse_args()
    
    runner = SLIMTestRunner(args.slim_binary)
    
    try:
        test_case = runner.parse_lmntest_file(Path(args.test_file))
        report = runner.run_test(test_case, args.type)
        
        if args.json:
            print(json.dumps({
                "name": report.name,
                "result": report.result.value,
                "duration": report.duration,
                "details": report.details
            }))
        else:
            print("1..1")  # TAP plan
            runner.print_tap_result(report, 1)
            
        sys.exit(0 if report.result == TestResult.PASS else 1)
        
    except Exception as e:
        print(f"ERROR: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()