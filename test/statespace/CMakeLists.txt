# State space tests
# Tests for non-deterministic execution

file(GLOB_RECURSE STATESPACE_CHECK_SCRIPTS "${CMAKE_CURRENT_SOURCE_DIR}/testsuite/*/check.sh")

foreach(check_script ${STATESPACE_CHECK_SCRIPTS})
    get_filename_component(test_dir ${check_script} DIRECTORY)
    get_filename_component(test_name ${test_dir} NAME)
    
    # Create wrapper script that checks slim_CHECK_ND
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
    
    set_tests_properties(statespace_${test_name} PROPERTIES
        ENVIRONMENT "SLIM_BINARY=${CMAKE_BINARY_DIR}/src/slim;LMNTAL_HOME=${LMNTAL_HOME}"
    )
endforeach()

# Statespace tests are enabled/disabled dynamically based on slim_CHECK_ND
# They are always defined but may be skipped at runtime