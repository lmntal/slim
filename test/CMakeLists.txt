# Testing framework for SLIM
# Based on the TAP (Test Anything Protocol) system used in the original

# Helper function to add TAP-based tests
function(add_tap_test test_name test_script)
    add_test(
        NAME ${test_name}
        COMMAND ${test_script}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    )
    set_tests_properties(${test_name} PROPERTIES
        ENVIRONMENT "PATH=${CMAKE_BINARY_DIR}/src:$ENV{PATH}"
    )
endfunction()

# Add subdirectories for different test types
add_subdirectory(system_check)
add_subdirectory(statespace) 
add_subdirectory(library_check)

# Set environment variables for tests
set(ENV{slim_CHECK_OPTIONS} "")

# Include enhanced test framework
include(${CMAKE_CURRENT_SOURCE_DIR}/enhanced_test_framework.cmake)

# Use enhanced test discovery and configuration - individual .lmntest files
add_lmntest_directory("system" "system_check")
add_lmntest_directory("statespace" "statespace")

# Add library tests that don't use .lmntest files
add_check_script_tests("library" "library_check")

# Create custom test targets
create_test_targets()

# test_config.py already exists - no need to configure

# Legacy compatibility - keep 'make check' working
add_custom_target(check
    COMMAND ${CMAKE_CTEST_COMMAND} --output-on-failure
    DEPENDS slim
    COMMENT "Running all tests (legacy compatibility)"
)