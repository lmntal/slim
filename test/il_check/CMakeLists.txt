# IL check tests
# Convert the TAP-based tests to CTest format

# Function to create test from .lmntest files
function(add_lmntest_suite suite_name test_files)
    # Create a test script for this suite
    set(test_script "${CMAKE_CURRENT_BINARY_DIR}/${suite_name}_test.sh")

    file(WRITE ${test_script} "#!/bin/bash\n")
    file(APPEND ${test_script} "set -e\n")
    file(APPEND ${test_script} "cd ${CMAKE_CURRENT_SOURCE_DIR}\n")
    file(APPEND ${test_script} "./check.pl")

    foreach(test_file ${test_files})
        file(APPEND ${test_script} " ${test_file}")
    endforeach()

    file(APPEND ${test_script} "\n")

    # Make script executable
    file(COPY ${test_script}
         DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
         FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)

    # Add the test
    add_test(NAME il_check_${suite_name} COMMAND ${test_script})
    set_tests_properties(il_check_${suite_name} PROPERTIES
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        ENVIRONMENT "SLIM_BINARY=${CMAKE_BINARY_DIR}/src/slim"
    )
endfunction()

# Find all check.sh files and create tests for them
file(GLOB_RECURSE CHECK_SCRIPTS "${CMAKE_CURRENT_SOURCE_DIR}/testsuite/*/check.sh")
foreach(check_script ${CHECK_SCRIPTS})
    get_filename_component(test_dir ${check_script} DIRECTORY)
    get_filename_component(test_name ${test_dir} NAME)

    add_test(NAME il_check_${test_name}
        COMMAND ${check_script}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    )

    set_tests_properties(il_check_${test_name} PROPERTIES
        ENVIRONMENT "SLIM_BINARY=${CMAKE_BINARY_DIR}/src/slim;LMNTAL_HOME=${LMNTAL_HOME}"
    )
endforeach()
