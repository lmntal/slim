add_library(zd_in_slim STATIC
    deflate.c      deflate.h
    infblock.c     infblock.h
    infcodes.c     infcodes.h
    inffast.c      inffast.h
    inffixed.h     inflate.c
    inftrees.c     inftrees.h
    infutil.c      infutil.h
    adler32.c
    tailor.h
    trees.c        trees.h
    zd_mem.c       zd_mem.h
    zdconf.h
    zdelta.c       zdlib.h
    zutil.c        zutil.h
)

target_include_directories(zd_in_slim PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
)