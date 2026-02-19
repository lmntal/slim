add_library(lmn_ffi STATIC
    jni_lmntal.cpp jni_lmntal.h
    lmntal_system_adapter.cpp lmntal_system_adapter.h
)

target_include_directories(lmn_ffi PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/..
    ${CMAKE_CURRENT_SOURCE_DIR}/../../third_party/zdelta-2.1
    ${CMAKE_BINARY_DIR}/src
)

target_link_libraries(lmn_ffi PRIVATE
    zd_in_slim
    Threads::Threads
)