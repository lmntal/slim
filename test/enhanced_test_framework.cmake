# Enhanced Testing Framework for SLIM
# Individual test file approach - creates separate CTest for each .lmntest file

# Helper function to auto-discover and add individual tests for each .lmntest file
function(add_lmntest_directory test_type test_dir)
    # Find all .lmntest files in test suites
    file(GLOB_RECURSE LMNTEST_FILES "${CMAKE_CURRENT_SOURCE_DIR}/${test_dir}/testsuite/*/*.lmntest")
    
    foreach(lmntest_file ${LMNTEST_FILES})
        # Extract test info from path
        get_filename_component(test_case_name ${lmntest_file} NAME_WE)
        get_filename_component(test_suite_dir ${lmntest_file} DIRECTORY)
        get_filename_component(test_suite ${test_suite_dir} NAME)
        
        # Create unique test name
        set(full_test_name "${test_type}_${test_suite}_${test_case_name}")
        
        # Find the check.sh script for this test suite
        set(check_script "${test_suite_dir}/check.sh")
        if(NOT EXISTS ${check_script})
            message(WARNING "No check.sh found for ${test_suite_dir}")
            continue()
        endif()
        
        # Handle statespace tests specially - they need slim_CHECK_ND wrapper
        if(test_type STREQUAL "statespace")
            # Create wrapper script that checks slim_CHECK_ND and runs single test
            set(wrapper_script "${CMAKE_CURRENT_BINARY_DIR}/${test_dir}/${test_suite}_${test_case_name}_wrapper.sh")
            file(WRITE ${wrapper_script} "#!/bin/bash\n")
            file(APPEND ${wrapper_script} "if [ \"$slim_CHECK_ND\" != \"yes\" ]; then\n")
            file(APPEND ${wrapper_script} "  echo \"1..0 # SKIP statespace tests require slim_CHECK_ND=yes\"\n")
            file(APPEND ${wrapper_script} "  exit 0\n")
            file(APPEND ${wrapper_script} "fi\n")
            file(APPEND ${wrapper_script} "cd ${CMAKE_CURRENT_SOURCE_DIR}/${test_dir}\n")
            file(APPEND ${wrapper_script} "./check.pl \"$SLIM_BINARY\" \"${CMAKE_CURRENT_SOURCE_DIR}/${test_dir}/testsuite/${test_suite}/${test_case_name}.il\"\n")
            
            # Make wrapper executable with proper permissions
            file(COPY ${wrapper_script}
                 DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/${test_dir}
                 FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
            
            # Add test using wrapper script
            add_test(NAME ${full_test_name} COMMAND ${wrapper_script} WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${test_dir})
        else()
            # Create individual test runner script for non-statespace tests
            set(individual_script "${CMAKE_CURRENT_BINARY_DIR}/${test_dir}/${test_suite}_${test_case_name}_run.sh")
            file(WRITE ${individual_script} "#!/bin/bash\n")
            file(APPEND ${individual_script} "cd ${CMAKE_CURRENT_SOURCE_DIR}/${test_dir}\n")
            file(APPEND ${individual_script} "# Inherit VERBOSE environment variable\n")
            file(APPEND ${individual_script} "export VERBOSE=\"\${VERBOSE}\"\n") 
            file(APPEND ${individual_script} "export V=\"\${V}\"\n")
            file(APPEND ${individual_script} "./check.pl \"$SLIM_BINARY\" \"${CMAKE_CURRENT_SOURCE_DIR}/${test_dir}/testsuite/${test_suite}/${test_case_name}.il\"\n")
            
            # Make script executable directly
            execute_process(COMMAND chmod +x ${individual_script})
            
            # Add test using individual script
            add_test(NAME ${full_test_name} COMMAND ${individual_script} WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${test_dir})
        endif()
        
        # Set test properties - VERBOSE/V will be inherited from environment
        set_tests_properties(${full_test_name} PROPERTIES
            ENVIRONMENT "SLIM_BINARY=$<TARGET_FILE:slim>;LMNTAL_HOME=${LMNTAL_HOME};slim_CHECK_OPTIONS=${slim_CHECK_OPTIONS};TEST_BUILD_DIR=${CMAKE_CURRENT_BINARY_DIR}/${test_dir}"
            TIMEOUT 30
            LABELS "${test_type};${test_suite};${test_case_name}"
        )
        
        # Add to appropriate groups
        if(test_type STREQUAL "statespace")
            set_tests_properties(${full_test_name} PROPERTIES
                TIMEOUT 300  # Longer timeout for model checking
                RESOURCE_LOCK "statespace_lock"  # Prevent parallel heavy tests
            )
        endif()
    endforeach()
endfunction()

# Helper function to add tests that don't use .lmntest files (like library tests)
function(add_check_script_tests test_type test_dir)
    file(GLOB CHECK_SCRIPTS "${CMAKE_CURRENT_SOURCE_DIR}/${test_dir}/testsuite/*/check.sh")
    
    foreach(check_script ${CHECK_SCRIPTS})
        # Extract test info from path
        get_filename_component(test_suite_dir ${check_script} DIRECTORY)
        get_filename_component(test_suite ${test_suite_dir} NAME)
        
        # Skip if this directory has .lmntest files (handled by add_lmntest_directory)
        file(GLOB LMNTEST_FILES "${test_suite_dir}/*.lmntest")
        if(LMNTEST_FILES)
            continue()
        endif()
        
        # Create unique test name
        set(full_test_name "${test_type}_${test_suite}")
        
        # Handle statespace tests specially - only heavy advanced tests need slim_CHECK_ND wrapper
        if(test_type STREQUAL "statespace")
            # Simple statespace tests (basic, hyperlink) should run without wrapper
            # Only advanced tests need the slim_CHECK_ND wrapper
            if(test_suite STREQUAL "advanced")
                # Check if the check.sh script already handles slim_CHECK_ND internally
                file(READ ${check_script} script_content)
                string(FIND "${script_content}" "slim_CHECK_ND" has_nd_check)
                
                if(has_nd_check GREATER_EQUAL 0)
                    # Script already handles slim_CHECK_ND - use it directly
                    add_test(NAME ${full_test_name} COMMAND ${check_script} WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${test_dir})
                else()
                    # Create wrapper for advanced tests that don't have their own check
                    set(wrapper_script "${CMAKE_CURRENT_BINARY_DIR}/${test_dir}/${test_suite}_wrapper.sh")
                    file(WRITE ${wrapper_script} "#!/bin/bash\n")
                    file(APPEND ${wrapper_script} "if [ \"$slim_CHECK_ND\" != \"yes\" ]; then\n")
                    file(APPEND ${wrapper_script} "  echo \"1..0 # SKIP statespace tests require slim_CHECK_ND=yes\"\n")
                    file(APPEND ${wrapper_script} "  exit 0\n")
                    file(APPEND ${wrapper_script} "fi\n")
                    file(APPEND ${wrapper_script} "exec ${check_script}\n")
                    
                    # Make wrapper executable with proper permissions
                    file(COPY ${wrapper_script}
                         DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/${test_dir}
                         FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
                    
                    # Add test using wrapper script
                    add_test(NAME ${full_test_name} COMMAND ${wrapper_script} WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${test_dir})
                endif()
            else()
                # Simple statespace tests (basic, hyperlink) - run directly
                add_test(NAME ${full_test_name} COMMAND ${check_script} WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${test_dir})
            endif()
        else()
            # Add test using original check.sh script  
            add_test(NAME ${full_test_name} COMMAND ${check_script} WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${test_dir})
        endif()
        
        # Set test properties - VERBOSE/V will be inherited from environment
        set_tests_properties(${full_test_name} PROPERTIES
            ENVIRONMENT "SLIM_BINARY=$<TARGET_FILE:slim>;LMNTAL_HOME=${LMNTAL_HOME};slim_CHECK_OPTIONS=${slim_CHECK_OPTIONS}"
            TIMEOUT 30
            LABELS "${test_type};${test_suite}"
        )
        
        # Add to appropriate groups
        if(test_type STREQUAL "statespace")
            set_tests_properties(${full_test_name} PROPERTIES
                TIMEOUT 300  # Longer timeout for model checking
                RESOURCE_LOCK "statespace_lock"  # Prevent parallel heavy tests
            )
        endif()
    endforeach()
endfunction()

# Custom test targets for different categories
function(create_test_targets)
    add_custom_target(test-quick
        COMMAND ${CMAKE_CTEST_COMMAND} -L "system|library" --output-on-failure
        COMMENT "Running quick tests (system + library)"
    )
    
    add_custom_target(test-performance
        COMMAND ${CMAKE_CTEST_COMMAND} -L "performance" --output-on-failure
        COMMENT "Running performance tests"
    )
    
    add_custom_target(test-parallel
        COMMAND ${CMAKE_CTEST_COMMAND} -L "parallel" --output-on-failure -j4
        COMMENT "Running parallel execution tests"  
    )
    
    add_custom_target(test-model-checking
        COMMAND ${CMAKE_CTEST_COMMAND} -L "statespace" --output-on-failure
        COMMENT "Running model checking tests"
    )
endfunction()