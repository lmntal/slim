# Performance benchmarking for SLIM

# Benchmark test cases
file(GLOB BENCHMARK_FILES "${CMAKE_CURRENT_SOURCE_DIR}/*.lmn")

foreach(benchmark_file ${BENCHMARK_FILES})
    get_filename_component(benchmark_name ${benchmark_file} NAME_WE)
    
    add_test(
        NAME benchmark_${benchmark_name}
        COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/run_benchmark.py
                --benchmark ${benchmark_file}
                --slim-binary $<TARGET_FILE:slim>
                --output ${CMAKE_CURRENT_BINARY_DIR}/benchmark_${benchmark_name}.json
    )
    
    set_tests_properties(benchmark_${benchmark_name} PROPERTIES
        LABELS "benchmark"
        TIMEOUT 300
    )
endforeach()

# Benchmark comparison target
add_custom_target(benchmark-compare
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/compare_benchmarks.py 
            --results-dir ${CMAKE_CURRENT_BINARY_DIR}
            --baseline ${CMAKE_CURRENT_SOURCE_DIR}/baseline.json
    COMMENT "Comparing benchmark results with baseline"
)