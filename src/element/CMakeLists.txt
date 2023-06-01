add_library(lmn_elm STATIC)

target_include_directories(lmn_elm PRIVATE
    ${CMAKE_SOURCE_DIR}/src
    ${CMAKE_SOURCE_DIR}/third_party
)

target_sources(lmn_elm PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/re2c/buffer.cpp
)

target_sources(lmn_elm PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/alloc.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/clock.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/error.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/instruction.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/internal_hash.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/life_time.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/lmnstring.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/lmntal_thread.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/memory_pool.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/port.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/process_util.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/st.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/util.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/vector.cpp
)