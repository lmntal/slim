add_library(unit_test STATIC
    sample_test.c
    tree_compress_test.c
    unit_test.c
)

target_include_directories(unit_test PRIVATE
    ${CMAKE_SOURCE_DIR}/src
)