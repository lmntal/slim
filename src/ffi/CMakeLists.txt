add_library(lmn_ffi STATIC)

target_include_directories(lmn_ffi PRIVATE
    ${CMAKE_SOURCE_DIR}/src
)

target_sources(lmn_ffi PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/jni_lmntal.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/lmntal_system_adapter.cpp
)

target_link_libraries(lmn_ffi PRIVATE
    lmn_elm
)