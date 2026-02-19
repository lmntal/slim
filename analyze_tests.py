#!/usr/bin/env python3
"""
Analyze and group CTest results from 208 individual tests
Group them according to original test suites for better organization
"""

import re
import sys
from collections import defaultdict, OrderedDict

def parse_ctest_output(filename):
    """Parse CTest output and extract test results"""
    tests = []
    
    with open(filename, 'r') as f:
        content = f.read()
    
    # Find all test results
    test_pattern = r'(\d+)/\d+\s+Test\s+#\d+:\s+(\S+)\s+\.+\s*(Passed|Failed)\s+([0-9.]+)\s+sec'
    matches = re.findall(test_pattern, content)
    
    for match in matches:
        test_num, test_name, status, duration = match
        tests.append({
            'number': int(test_num),
            'name': test_name,
            'status': status,
            'duration': float(duration)
        })
    
    return tests

def group_tests_by_suite(tests):
    """Group individual tests by their test suite"""
    grouped = defaultdict(list)
    
    for test in tests:
        name = test['name']
        
        # Extract suite name from test name
        # Examples:
        # system_append_append1 -> system_append
        # system_basic_basic1 -> system_basic  
        # library_integer -> library_integer
        
        if '_' in name:
            parts = name.split('_')
            if len(parts) >= 3:
                # system_append_append1 -> system_append
                suite = '_'.join(parts[:2])
            elif len(parts) == 2:
                # library_integer -> library_integer
                suite = name
            else:
                suite = name
        else:
            suite = name
            
        grouped[suite].append(test)
    
    return grouped

def analyze_suite(suite_name, tests):
    """Analyze a single test suite"""
    total = len(tests)
    passed = len([t for t in tests if t['status'] == 'Passed'])
    failed = len([t for t in tests if t['status'] == 'Failed'])
    total_time = sum(t['duration'] for t in tests)
    
    return {
        'name': suite_name,
        'total': total,
        'passed': passed,
        'failed': failed,
        'success_rate': (passed / total * 100) if total > 0 else 0,
        'total_time': total_time,
        'tests': tests
    }

def print_grouped_analysis(grouped_tests):
    """Print comprehensive grouped analysis"""
    
    print("=" * 80)
    print("SLIM TEST ANALYSIS - 208 Individual Tests Grouped by Suite")
    print("=" * 80)
    
    # Sort suites by category (system, library, statespace)
    suite_order = []
    for suite in sorted(grouped_tests.keys()):
        if suite.startswith('system_'):
            suite_order.append(suite)
    for suite in sorted(grouped_tests.keys()):
        if suite.startswith('library_'):
            suite_order.append(suite)
    for suite in sorted(grouped_tests.keys()):
        if suite.startswith('statespace_'):
            suite_order.append(suite)
    
    # Add any remaining suites
    for suite in sorted(grouped_tests.keys()):
        if suite not in suite_order:
            suite_order.append(suite)
    
    total_tests = 0
    total_passed = 0
    total_failed = 0
    
    for suite_name in suite_order:
        tests = grouped_tests[suite_name]
        analysis = analyze_suite(suite_name, tests)
        
        total_tests += analysis['total']
        total_passed += analysis['passed']
        total_failed += analysis['failed']
        
        # Print suite header
        status_icon = "✅" if analysis['failed'] == 0 else "❌"
        print(f"\n{status_icon} {analysis['name'].upper()}")
        print(f"   Tests: {analysis['passed']}/{analysis['total']} passed "
              f"({analysis['success_rate']:.1f}%) | Time: {analysis['total_time']:.2f}s")
        
        # Print individual test details if there are failures
        if analysis['failed'] > 0:
            print("   Failed tests:")
            for test in analysis['tests']:
                if test['status'] == 'Failed':
                    print(f"     ❌ {test['name']} ({test['duration']:.2f}s)")
        
        # Print all tests for small suites or if requested
        if analysis['total'] <= 5 or analysis['failed'] > 0:
            print("   Individual tests:")
            for test in sorted(analysis['tests'], key=lambda x: x['name']):
                icon = "✅" if test['status'] == 'Passed' else "❌"
                print(f"     {icon} {test['name']} ({test['duration']:.2f}s)")
    
    # Summary
    print("\n" + "=" * 80)
    print("OVERALL SUMMARY")
    print("=" * 80)
    print(f"Total Tests: {total_tests}")
    print(f"Passed: {total_passed} ({total_passed/total_tests*100:.1f}%)")
    print(f"Failed: {total_failed} ({total_failed/total_tests*100:.1f}%)")
    print(f"Test Suites: {len(suite_order)}")
    
    if total_failed == 0:
        print("\n🎉 ALL TESTS PASSING! CMake migration successful! 🚀")
    else:
        print(f"\n⚠️  {total_failed} tests need attention")

def main():
    if len(sys.argv) != 2:
        print("Usage: python3 analyze_tests.py <ctest_output_file>")
        print("\nExample:")
        print("  cd build")
        print("  ctest --output-on-failure > test_results.txt 2>&1")
        print("  python3 ../analyze_tests.py test_results.txt")
        sys.exit(1)
    
    filename = sys.argv[1]
    
    try:
        tests = parse_ctest_output(filename)
        if not tests:
            print("No tests found in the output file. Make sure you ran:")
            print("  ctest --output-on-failure > test_results.txt 2>&1")
            sys.exit(1)
            
        grouped_tests = group_tests_by_suite(tests)
        print_grouped_analysis(grouped_tests)
        
    except FileNotFoundError:
        print(f"Error: File '{filename}' not found")
        print("Make sure to run ctest first:")
        print("  cd build && ctest --output-on-failure > test_results.txt 2>&1")
        sys.exit(1)
    except Exception as e:
        print(f"Error analyzing test results: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()