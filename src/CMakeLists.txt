configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/cmake_arch.h.in
    ${CMAKE_CURRENT_SOURCE_DIR}/arch.h
)

configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/cmake_config.h.in
    ${CMAKE_CURRENT_SOURCE_DIR}/config.h
)

add_executable(slim)

target_compile_definitions(slim PRIVATE FMT_HEADER_ONLY)

target_sources(slim PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/main.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/env.cpp
)

target_sources(slim PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/ext/array.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/ext/atom.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/ext/float.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/ext/init_exts.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/ext/initial_ruleset.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/ext/integer.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/ext/membrane.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/ext/nd_conf.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/ext/nlmem.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/ext/react_rule.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/ext/set.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/ext/state_map.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/ext/time.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/ext/zerostep.cpp
)

target_include_directories(slim PRIVATE ${CMAKE_SOURCE_DIR}/src)
target_include_directories(slim PRIVATE ${CMAKE_SOURCE_DIR}/third_party)

target_link_libraries(slim PRIVATE Threads::Threads)
target_link_libraries(slim PRIVATE OpenMP::OpenMP_CXX)
target_link_libraries(slim PRIVATE lmn_elm)
target_link_libraries(slim PRIVATE lmn_ffi)
target_link_libraries(slim PRIVATE lmn_loader)
target_link_libraries(slim PRIVATE lmn_verifier)
target_link_libraries(slim PRIVATE lmn_vm)

execute_process(
    COMMAND
    git show -s --format=%h
    RESULT_VARIABLE
    SHORT_HASH_RESULT
    OUTPUT_VARIABLE
    SHORT_HASH)
string(STRIP ${SHORT_HASH} SHORT_HASH)

# Add defines
target_compile_definitions(slim PRIVATE COMMIT_ID="${SHORT_HASH}")

if(USE_JNI)
    find_package(JNI REQUIRED)
    target_link_libraries(slim PRIVATE ${JNI_LIBRARIES})
    target_include_directories(slim PRIVATE ${JNI_INCLUDE_DIRS})
endif()

if(HAVE_GPERFTOOLS)
    target_link_libraries(slim PRIVATE tcmalloc)
elseif(HAVE_TCMALLOC)
    target_link_libraries(slim PRIVATE tcmalloc_minimal)
endif()

if(USE_CUNIT)
    target_include_directories(slim PRIVATE ${CUNIT_HOME}/include)
    target_link_directories(slim PRIVATE ${CUNIT_HOME}/lib)
    target_link_libraries(slim PRIVATE cunit)

    add_subdirectory(test)

    target_link_libraries(slim PRIVATE unit_test)
endif()

add_subdirectory(element)
add_subdirectory(ffi)
add_subdirectory(loader)
add_subdirectory(verifier)
add_subdirectory(vm)

set_property(
    TARGET lmn_verifier
    APPEND
    PROPERTY ADDITIONAL_CLEAN_FILES ${CMAKE_CURRENT_SOURCE_DIR}/arch.h ${CMAKE_CURRENT_SOURCE_DIR}/config.h
)