# Library check tests  
# Tests for built-in library functions

# Function to compile .lmn files to .il files for library tests
function(compile_lmn_to_il lmn_file)
    get_filename_component(test_dir ${lmn_file} DIRECTORY)
    get_filename_component(base_name ${lmn_file} NAME_WE)
    
    # Create unique target name using relative path
    file(RELATIVE_PATH rel_path ${CMAKE_CURRENT_SOURCE_DIR} ${lmn_file})
    string(REPLACE "/" "_" unique_name ${rel_path})
    string(REPLACE ".lmn" "" unique_name ${unique_name})
    
    set(il_file "${test_dir}/${base_name}.il")
    
    # Add custom command to compile .lmn to .il
    add_custom_command(
        OUTPUT ${il_file}
        COMMAND ${LMNTAL_HOME}/bin/lmntal --slimcode ${lmn_file} > ${il_file}
        DEPENDS ${lmn_file}
        WORKING_DIRECTORY ${test_dir}
        COMMENT "Compiling ${lmn_file} to ${il_file}"
    )
    
    # Create a target for this compilation with unique name
    add_custom_target(compile_lib_${unique_name} DEPENDS ${il_file})
    
    # Set a property to store the unique target name for later use
    set_property(GLOBAL PROPERTY "COMPILE_LIB_TARGET_${lmn_file}" compile_lib_${unique_name})
endfunction()

# Find all .lmn files and compile them
file(GLOB_RECURSE LIBRARY_LMN_FILES "${CMAKE_CURRENT_SOURCE_DIR}/testsuite/*/*.lmn")
foreach(lmn_file ${LIBRARY_LMN_FILES})
    compile_lmn_to_il(${lmn_file})
endforeach()

# Create a target that compiles all .lmn files
add_custom_target(compile_all_library_lmn)
foreach(lmn_file ${LIBRARY_LMN_FILES})
    get_property(compile_target GLOBAL PROPERTY "COMPILE_LIB_TARGET_${lmn_file}")
    add_dependencies(compile_all_library_lmn ${compile_target})
endforeach()

file(GLOB_RECURSE LIBRARY_CHECK_SCRIPTS "${CMAKE_CURRENT_SOURCE_DIR}/testsuite/*/check.sh")

foreach(check_script ${LIBRARY_CHECK_SCRIPTS})
    get_filename_component(test_dir ${check_script} DIRECTORY)
    get_filename_component(test_name ${test_dir} NAME)
    
    # Find all .lmn files in this test directory
    file(GLOB test_lmn_files "${test_dir}/*.lmn")
    set(compile_targets "")
    foreach(lmn_file ${test_lmn_files})
        get_property(compile_target GLOBAL PROPERTY "COMPILE_LIB_TARGET_${lmn_file}")
        if(compile_target)
            list(APPEND compile_targets ${compile_target})
        endif()
    endforeach()
    
    add_test(NAME library_${test_name}
        COMMAND ${check_script}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    )
    
    set_tests_properties(library_${test_name} PROPERTIES
        ENVIRONMENT "SLIM_BINARY=${CMAKE_BINARY_DIR}/bin/slim;LMNTAL_HOME=${LMNTAL_HOME}"
        FIXTURES_REQUIRED "compile_lib_${test_name}"
    )
    
    # Add a fixture setup that compiles the .lmn files for this test
    if(compile_targets)
        add_test(NAME compile_lib_${test_name}
            COMMAND ${CMAKE_COMMAND} --build ${CMAKE_BINARY_DIR} --target ${compile_targets}
        )
        set_tests_properties(compile_lib_${test_name} PROPERTIES
            FIXTURES_SETUP "compile_lib_${test_name}"
        )
    endif()
endforeach()