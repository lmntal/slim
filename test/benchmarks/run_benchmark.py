#!/usr/bin/env python3
"""
Performance benchmark runner for SLIM
"""

import argparse
import json
import time
import subprocess
import statistics
import sys
from pathlib import Path

def run_benchmark(slim_binary, lmn_file, num_runs=5):
    """Run benchmark multiple times and collect statistics"""
    
    # Compile LMNtal file to IL
    compile_cmd = ["lmntal", "--slimcode", str(lmn_file)]
    compile_result = subprocess.run(compile_cmd, capture_output=True, text=True)
    
    if compile_result.returncode != 0:
        return None, f"Compilation failed: {compile_result.stderr}"
    
    il_code = compile_result.stdout
    
    # Create temporary IL file
    il_file = Path(f"/tmp/{lmn_file.stem}.il")
    with open(il_file, 'w') as f:
        f.write(il_code)
    
    execution_times = []
    memory_usage = []
    
    try:
        for i in range(num_runs):
            # Run SLIM with time and memory measurement
            cmd = ["time", "-v", slim_binary, str(il_file)]
            start_time = time.time()
            
            result = subprocess.run(cmd, capture_output=True, text=True)
            
            end_time = time.time()
            execution_time = end_time - start_time
            execution_times.append(execution_time)
            
            # Parse memory usage from time output
            for line in result.stderr.split('\n'):
                if "Maximum resident set size" in line:
                    memory_kb = int(line.split(':')[1].strip())
                    memory_usage.append(memory_kb)
                    break
    
    finally:
        il_file.unlink()
    
    if not execution_times:
        return None, "No successful runs"
    
    return {
        "benchmark": lmn_file.name,
        "runs": num_runs,
        "execution_times": execution_times,
        "memory_usage": memory_usage,
        "statistics": {
            "mean_time": statistics.mean(execution_times),
            "median_time": statistics.median(execution_times),
            "stdev_time": statistics.stdev(execution_times) if len(execution_times) > 1 else 0,
            "min_time": min(execution_times),
            "max_time": max(execution_times),
            "mean_memory": statistics.mean(memory_usage) if memory_usage else 0,
            "max_memory": max(memory_usage) if memory_usage else 0
        }
    }, None

def main():
    parser = argparse.ArgumentParser(description="Run SLIM performance benchmark")
    parser.add_argument("--benchmark", required=True, help="LMNtal benchmark file")
    parser.add_argument("--slim-binary", required=True, help="Path to SLIM executable")
    parser.add_argument("--output", required=True, help="Output JSON file")
    parser.add_argument("--runs", type=int, default=5, help="Number of runs")
    
    args = parser.parse_args()
    
    benchmark_file = Path(args.benchmark)
    if not benchmark_file.exists():
        print(f"Benchmark file not found: {benchmark_file}")
        sys.exit(1)
    
    results, error = run_benchmark(args.slim_binary, benchmark_file, args.runs)
    
    if error:
        print(f"Benchmark failed: {error}")
        sys.exit(1)
    
    with open(args.output, 'w') as f:
        json.dump(results, f, indent=2)
    
    print(f"Benchmark completed: {results['statistics']['mean_time']:.3f}s ± {results['statistics']['stdev_time']:.3f}s")

if __name__ == "__main__":
    main()