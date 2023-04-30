add_library(zd_in_slim STATIC)

target_sources(zd_in_slim PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/deflate.c
    ${CMAKE_CURRENT_SOURCE_DIR}/infblock.c
    ${CMAKE_CURRENT_SOURCE_DIR}/infcodes.c
    ${CMAKE_CURRENT_SOURCE_DIR}/inffast.c
    ${CMAKE_CURRENT_SOURCE_DIR}/inflate.c
    ${CMAKE_CURRENT_SOURCE_DIR}/inftrees.c
    ${CMAKE_CURRENT_SOURCE_DIR}/infutil.c
    ${CMAKE_CURRENT_SOURCE_DIR}/adler32.c
    ${CMAKE_CURRENT_SOURCE_DIR}/trees.c
    ${CMAKE_CURRENT_SOURCE_DIR}/zd_mem.c
    ${CMAKE_CURRENT_SOURCE_DIR}/zdelta.c
    ${CMAKE_CURRENT_SOURCE_DIR}/zutil.c
)