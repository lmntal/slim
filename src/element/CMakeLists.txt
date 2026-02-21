add_library(lmn_elm STATIC
    alloc.cpp
    re2c/buffer.cpp
    clock.cpp clock.h
    element.h
    error.cpp error.h
    file_util.cpp file_util.h
    instruction.cpp
    internal_hash.cpp internal_hash.h
    lmnstring.cpp lmnstring.h
    lmntal_thread.cpp lmntal_thread.h
    memory_pool.cpp memory_pool.h
    port.cpp port.h
    process_util.cpp process_util.h
    queue.cpp queue.h
    st.cpp st.h
    util.cpp util.h
    vector.cpp vector.h
    life_time.cpp
)

target_include_directories(lmn_elm PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/..
    ${CMAKE_CURRENT_SOURCE_DIR}/../../third_party/zdelta-2.1
    ${CMAKE_BINARY_DIR}/src
)

target_link_libraries(lmn_elm PRIVATE
    Threads::Threads
)