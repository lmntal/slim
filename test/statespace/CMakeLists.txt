# State space tests
# Tests for non-deterministic execution

# Function to compile .lmn files to .il files for statespace tests
function(compile_statespace_lmn_to_il lmn_file)
    get_filename_component(test_dir ${lmn_file} DIRECTORY)
    get_filename_component(base_name ${lmn_file} NAME_WE)
    
    set(il_file "${test_dir}/${base_name}.il")
    
    # Add custom command to compile .lmn to .il
    add_custom_command(
        OUTPUT ${il_file}
        COMMAND ${LMNTAL_HOME}/bin/lmntal --slimcode ${lmn_file} > ${il_file}
        DEPENDS ${lmn_file}
        WORKING_DIRECTORY ${test_dir}
        COMMENT "Compiling statespace ${lmn_file} to ${il_file}"
    )
endfunction()

# Find and compile all .lmn files in statespace tests
file(GLOB_RECURSE STATESPACE_LMN_FILES "${CMAKE_CURRENT_SOURCE_DIR}/testsuite/*/*.lmn")
foreach(lmn_file ${STATESPACE_LMN_FILES})
    compile_statespace_lmn_to_il(${lmn_file})
endforeach()

file(GLOB_RECURSE STATESPACE_CHECK_SCRIPTS "${CMAKE_CURRENT_SOURCE_DIR}/testsuite/*/check.sh")

foreach(check_script ${STATESPACE_CHECK_SCRIPTS})
    get_filename_component(test_dir ${check_script} DIRECTORY)
    get_filename_component(test_name ${test_dir} NAME)
    
    # Check if this test already handles slim_CHECK_ND or if it's a simple test
    file(READ ${check_script} script_content)
    string(FIND "${script_content}" "slim_CHECK_ND" has_nd_check)
    
    # Find .il files that need to be compiled for this test
    file(GLOB test_lmn_files "${test_dir}/*.lmn")
    set(il_files "")
    foreach(lmn_file ${test_lmn_files})
        get_filename_component(base_name ${lmn_file} NAME_WE)
        list(APPEND il_files "${test_dir}/${base_name}.il")
    endforeach()
    
    if(has_nd_check GREATER_EQUAL 0 OR test_name STREQUAL "advanced")
        # Heavy tests that need slim_CHECK_ND - use wrapper or script's own check
        if(test_name STREQUAL "advanced")
            # Create wrapper script that properly handles skip status
            set(wrapper_script "${CMAKE_CURRENT_BINARY_DIR}/${test_name}_skip_wrapper.sh")
            file(WRITE ${wrapper_script} "#!/bin/bash\n")
            file(APPEND ${wrapper_script} "if [ \"$slim_CHECK_ND\" != \"yes\" ]; then\n")
            file(APPEND ${wrapper_script} "  echo \"1..0 # SKIP statespace tests require slim_CHECK_ND=yes\"\n")
            file(APPEND ${wrapper_script} "  exit 77\n")  # Exit code 77 = skipped in CTest
            file(APPEND ${wrapper_script} "fi\n")
            file(APPEND ${wrapper_script} "exec ${check_script}\n")
            
            # Make wrapper executable
            file(COPY ${wrapper_script} 
                 DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
                 FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
            
            add_test(NAME statespace_${test_name}
                COMMAND ${wrapper_script}
                WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
            )
            # Set properties to handle skip exit code properly
            set_tests_properties(statespace_${test_name} PROPERTIES
                SKIP_RETURN_CODE 77
            )
        else()
            # Create wrapper script for other heavy tests
            set(wrapper_script "${CMAKE_CURRENT_BINARY_DIR}/${test_name}_wrapper.sh")
            file(WRITE ${wrapper_script} "#!/bin/bash\n")
            file(APPEND ${wrapper_script} "if [ \"$slim_CHECK_ND\" != \"yes\" ]; then\n")
            file(APPEND ${wrapper_script} "  echo \"1..0 # SKIP statespace tests require slim_CHECK_ND=yes\"\n")
            file(APPEND ${wrapper_script} "  exit 0\n")
            file(APPEND ${wrapper_script} "fi\n")
            file(APPEND ${wrapper_script} "exec ${check_script}\n")
            
            # Make wrapper executable
            file(COPY ${wrapper_script} 
                 DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
                 FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
            
            add_test(NAME statespace_${test_name}
                COMMAND ${wrapper_script}
                WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
            )
        endif()
    else()
        # Simple statespace tests (basic, hyperlink) - run directly
        add_test(NAME statespace_${test_name}
            COMMAND ${check_script}
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        )
    endif()
    
    # Add dependencies on .il files if they exist
    if(il_files)
        set_tests_properties(statespace_${test_name} PROPERTIES
            ENVIRONMENT "SLIM_BINARY=${CMAKE_BINARY_DIR}/src/slim;LMNTAL_HOME=${LMNTAL_HOME}"
            DEPENDS "${il_files}"
        )
    else()
        set_tests_properties(statespace_${test_name} PROPERTIES
            ENVIRONMENT "SLIM_BINARY=${CMAKE_BINARY_DIR}/src/slim;LMNTAL_HOME=${LMNTAL_HOME}"
        )
    endif()
endforeach()

# Statespace tests are enabled/disabled dynamically based on slim_CHECK_ND
# They are always defined but may be skipped at runtime