# System check tests
# Convert the TAP-based tests to CTest format

# Set compiler flags for LMNtal
set(LMNC_FLAGS "--hl-opt --slimcode -O3")

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
    add_test(NAME system_${suite_name} COMMAND ${test_script})
    set_tests_properties(system_${suite_name} PROPERTIES
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        ENVIRONMENT "SLIM_BINARY=${CMAKE_BINARY_DIR}/src/slim"
    )
endfunction()

# Function to extract LMNtal source from .lmntest file and compile to .il
function(compile_lmntest_to_il lmntest_file)
    get_filename_component(test_dir ${lmntest_file} DIRECTORY)
    get_filename_component(base_name ${lmntest_file} NAME_WE)
    
    # Create unique target name using relative path
    file(RELATIVE_PATH rel_path ${CMAKE_CURRENT_SOURCE_DIR} ${lmntest_file})
    string(REPLACE "/" "_" unique_name ${rel_path})
    string(REPLACE ".lmntest" "" unique_name ${unique_name})
    
    set(il_file "${test_dir}/${base_name}.il")
    set(lmn_file "${test_dir}/${base_name}.lmn")
    
    # Add custom command to extract first line and compile
    add_custom_command(
        OUTPUT ${il_file}
        COMMAND head -1 ${lmntest_file} > ${lmn_file}
        COMMAND ${LMNTAL_HOME}/bin/lmntal --slimcode ${lmn_file} > ${il_file}
        COMMAND rm -f ${lmn_file}
        DEPENDS ${lmntest_file}
        WORKING_DIRECTORY ${test_dir}
        COMMENT "Compiling ${lmntest_file} to ${il_file}"
    )
    
    # Create a target for this compilation with unique name
    add_custom_target(compile_${unique_name} DEPENDS ${il_file})
    
    # Set a property to store the unique target name for later use
    set_property(GLOBAL PROPERTY "COMPILE_TARGET_${lmntest_file}" compile_${unique_name})
endfunction()

# Find all .lmntest files and compile them
file(GLOB_RECURSE LMNTEST_FILES "${CMAKE_CURRENT_SOURCE_DIR}/testsuite/*/*.lmntest")
foreach(lmntest_file ${LMNTEST_FILES})
    compile_lmntest_to_il(${lmntest_file})
endforeach()

# Create a target that compiles all .lmntest files
add_custom_target(compile_all_lmntest)
foreach(lmntest_file ${LMNTEST_FILES})
    get_property(compile_target GLOBAL PROPERTY "COMPILE_TARGET_${lmntest_file}")
    add_dependencies(compile_all_lmntest ${compile_target})
endforeach()

# Find all check.sh files and create tests for them
file(GLOB_RECURSE CHECK_SCRIPTS "${CMAKE_CURRENT_SOURCE_DIR}/testsuite/*/check.sh")
foreach(check_script ${CHECK_SCRIPTS})
    get_filename_component(test_dir ${check_script} DIRECTORY)
    get_filename_component(test_name ${test_dir} NAME)
    
    # Find all .lmntest files in this test directory
    file(GLOB test_lmntest_files "${test_dir}/*.lmntest")
    set(compile_targets "")
    foreach(lmntest_file ${test_lmntest_files})
        get_property(compile_target GLOBAL PROPERTY "COMPILE_TARGET_${lmntest_file}")
        if(compile_target)
            list(APPEND compile_targets ${compile_target})
        endif()
    endforeach()
    
    add_test(NAME system_${test_name}
        COMMAND ${check_script}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    )
    
    set_tests_properties(system_${test_name} PROPERTIES
        ENVIRONMENT "SLIM_BINARY=${CMAKE_BINARY_DIR}/src/slim;LMNTAL_HOME=${LMNTAL_HOME}"
        FIXTURES_REQUIRED "compile_${test_name}"
    )
    
    # Add a fixture setup that compiles the .lmntest files for this test
    if(compile_targets)
        add_test(NAME compile_${test_name}
            COMMAND ${CMAKE_COMMAND} --build ${CMAKE_BINARY_DIR} --target ${compile_targets}
        )
        set_tests_properties(compile_${test_name} PROPERTIES
            FIXTURES_SETUP "compile_${test_name}"
        )
    endif()
endforeach()