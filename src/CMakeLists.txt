add_subdirectory(verifier)
add_subdirectory(vm) 
add_subdirectory(loader)
add_subdirectory(element)
add_subdirectory(ffi)

if(ENABLE_CUNIT)
    add_subdirectory(test)
endif()

# Generate arch.h from template
configure_file(
    ${CMAKE_SOURCE_DIR}/cmake/arch.h.in
    ${CMAKE_CURRENT_SOURCE_DIR}/arch.h
    @ONLY
)

# Main slim executable
set(SLIM_SOURCES
    main.cpp
    env.cpp
    lmntal.h
    arch.h
    ext/init_exts.cpp
    ext/integer.cpp
    ext/float.cpp
    ext/nlmem.cpp
    ext/mell.cpp
    ext/array.cpp
    ext/array.h
    ext/atom.cpp
    ext/react_rule.cpp
    ext/initial_ruleset.cpp
    ext/nd_conf.cpp
    ext/set.cpp
    ext/set.h
    ext/state_map.cpp
    ext/state_map.h
    ext/membrane.cpp
    ext/membrane.h
    ext/zerostep.cpp
    ext/time.cpp
)

add_executable(slim ${SLIM_SOURCES})

# No need for add_dependencies since configure_file runs at configure time

target_include_directories(slim PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/../third_party/zdelta-2.1
    ${CMAKE_BINARY_DIR}/src
)

target_compile_definitions(slim PRIVATE
    COMMIT_ID="${GIT_COMMIT_ID}"
)

target_link_libraries(slim PRIVATE
    lmn_loader
    lmn_verifier  
    lmn_vm
    lmn_ffi
    lmn_elm
    zd_in_slim
    ${LIBDL_LIBRARY}
    Threads::Threads
)

if(LIBM_LIBRARY)
    target_link_libraries(slim PRIVATE ${LIBM_LIBRARY})
endif()

if(LIBZ_LIBRARY)
    target_link_libraries(slim PRIVATE ${LIBZ_LIBRARY})
endif()

if(LIBRT_LIBRARY)
    target_link_libraries(slim PRIVATE ${LIBRT_LIBRARY})
endif()

if(OpenMP_FOUND)
    target_link_libraries(slim PRIVATE OpenMP::OpenMP_C OpenMP::OpenMP_CXX)
endif()

if(ENABLE_JNI)
    target_link_libraries(slim PRIVATE jvm)
endif()

if(ENABLE_GPERFTOOLS)
    target_link_libraries(slim PRIVATE tcmalloc)
elseif(ENABLE_TCMALLOC)
    target_link_libraries(slim PRIVATE tcmalloc_minimal)
endif()

if(ENABLE_CUNIT)
    target_include_directories(slim PRIVATE ${CUNIT_HOME}/include)
    target_link_directories(slim PRIVATE ${CUNIT_HOME}/lib)
    target_link_libraries(slim PRIVATE cunit unit_test)
endif()