# Library check tests  
# Tests for built-in library functions

file(GLOB_RECURSE LIBRARY_CHECK_SCRIPTS "${CMAKE_CURRENT_SOURCE_DIR}/testsuite/*/check.sh")

foreach(check_script ${LIBRARY_CHECK_SCRIPTS})
    get_filename_component(test_dir ${check_script} DIRECTORY)
    get_filename_component(test_name ${test_dir} NAME)
    
    add_test(NAME library_${test_name}
        COMMAND ${check_script}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    )
    
    set_tests_properties(library_${test_name} PROPERTIES
        ENVIRONMENT "SLIM_BINARY=${CMAKE_BINARY_DIR}/bin/slim;LMNTAL_HOME=${LMNTAL_HOME}"
    )
endforeach()