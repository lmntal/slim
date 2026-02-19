# Custom commands for parser generation
add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/nc_parser.cpp ${CMAKE_CURRENT_BINARY_DIR}/nc_parser.hpp
    COMMAND ${BISON_EXECUTABLE} -d -p nc -o ${CMAKE_CURRENT_BINARY_DIR}/nc_parser.cpp ${CMAKE_CURRENT_SOURCE_DIR}/nc_parser.ypp
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/nc_parser.ypp
    COMMENT "Generating nc_parser.cpp"
)

add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/nc_lexer.cpp
    COMMAND ${RE2C_EXECUTABLE} -o ${CMAKE_CURRENT_BINARY_DIR}/nc_lexer.cpp ${CMAKE_CURRENT_SOURCE_DIR}/nc_lexer.cpp.re
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/nc_lexer.cpp.re
    COMMENT "Generating nc_lexer.cpp"
)

add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/propsym_parser.cpp ${CMAKE_CURRENT_BINARY_DIR}/propsym_parser.hpp
    COMMAND ${BISON_EXECUTABLE} -d -p propsym -o ${CMAKE_CURRENT_BINARY_DIR}/propsym_parser.cpp ${CMAKE_CURRENT_SOURCE_DIR}/propsym_parser.ypp
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/propsym_parser.ypp  
    COMMENT "Generating propsym_parser.cpp"
)

add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/propsym_lexer.cpp
    COMMAND ${RE2C_EXECUTABLE} -c -o ${CMAKE_CURRENT_BINARY_DIR}/propsym_lexer.cpp ${CMAKE_CURRENT_SOURCE_DIR}/propsym_lexer.cpp.re
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/propsym_lexer.cpp.re
    COMMENT "Generating propsym_lexer.cpp"
)

add_library(lmn_verifier STATIC
    automata.cpp automata.h
    binstr_compress.cpp binstr_compress.h
    delta_membrane.cpp delta_membrane.h
    dpor.cpp dpor.h
    dpor_naive.cpp dpor_naive.h
    ltl2ba_adapter.cpp ltl2ba_adapter.h
    mc.cpp mc.h
    mc_explorer.cpp mc_explorer.h
    mc_generator.cpp mc_generator.h
    mc_visualizer.cpp mc_visualizer.h
    mc_worker.cpp mc_worker.h
    mem_encode.cpp mem_encode.h
    mhash.cpp mhash.h
    ${CMAKE_CURRENT_BINARY_DIR}/nc_parser.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/nc_lexer.cpp
    propositional_symbol.cpp propositional_symbol.h
    ${CMAKE_CURRENT_BINARY_DIR}/propsym_parser.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/propsym_lexer.cpp
    runtime_status.cpp runtime_status.h
    state.cpp state.h state.hpp
    statespace.cpp statespace.h
    state_table.cpp state_table.hpp
    tree_compress.cpp tree_compress.h
    visitlog.h
    stack_macro.h
    state_dumper.cpp state_dumper.h
    mem_encode/decoder.cpp
    mem_encode/binstr.cpp
    verifier.h
    nc_lexer.hpp
    propsym_lexer.hpp
    state_defs.h
)

target_include_directories(lmn_verifier PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/..
    ${CMAKE_CURRENT_SOURCE_DIR}/../../third_party/zdelta-2.1
    ${CMAKE_CURRENT_BINARY_DIR}
    ${CMAKE_BINARY_DIR}/src
)

target_link_libraries(lmn_verifier PRIVATE
    zd_in_slim
    Threads::Threads
)

if(OpenMP_FOUND)
    target_link_libraries(lmn_verifier PRIVATE OpenMP::OpenMP_CXX)
endif()