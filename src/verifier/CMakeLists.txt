add_library(lmn_verifier STATIC)

set(NC_LEXER_INPUT ${CMAKE_CURRENT_SOURCE_DIR}/nc_lexer.cpp.re)
set(NC_LEXER_OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/nc_lexer.cpp)
set(NC_PARSER_OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/nc_parser.cpp)
set(NC_PARSER_DEF ${CMAKE_CURRENT_SOURCE_DIR}/nc_parser.hpp)

set(PROPSYM_LEXER_INPUT ${CMAKE_CURRENT_SOURCE_DIR}/propsym_lexer.cpp.re)
set(PROPSYM_LEXER_OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/propsym_lexer.cpp)
set(PROPSYM_PARSER_OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/propsym_parser.cpp)
set(PROPSYM_PARSER_DEF ${CMAKE_CURRENT_SOURCE_DIR}/propsym_parser.hpp)

bison_target(
    PARSER ${CMAKE_CURRENT_SOURCE_DIR}/nc_parser.ypp ${NC_PARSER_OUTPUT}
    DEFINES_FILE ${NC_PARSER_DEF}
    COMPILE_FLAGS "-d -p nc"
)

bison_target(
    PARSER ${CMAKE_CURRENT_SOURCE_DIR}/propsym_parser.ypp ${PROPSYM_PARSER_OUTPUT}
    DEFINES_FILE ${PROPSYM_PARSER_DEF}
    COMPILE_FLAGS "-d -p propsym"
)

target_include_directories(lmn_verifier PRIVATE
    ${CMAKE_SOURCE_DIR}/src
    ${CMAKE_CURRENT_SOURCE_DIR}
)

target_sources(lmn_verifier PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/mem_encode/binstr.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/mem_encode/decoder.cpp
)

target_sources(lmn_verifier PRIVATE
    ${NC_LEXER_OUTPUT}
    ${NC_PARSER_OUTPUT}
    ${PROPSYM_LEXER_OUTPUT}
    ${PROPSYM_PARSER_OUTPUT}
)

target_sources(lmn_verifier PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/automata.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/binstr_compress.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/delta_membrane.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/dpor.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/dpor_naive.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/ltl2ba_adapter.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/mc.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/mc_explorer.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/mc_generator.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/mc_visualizer.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/mc_worker.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/mem_encode.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/mhash.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/nc_lexer.cpp.re
    ${CMAKE_CURRENT_SOURCE_DIR}/propositional_symbol.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/propsym_lexer.cpp.re
    ${CMAKE_CURRENT_SOURCE_DIR}/runtime_status.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/state.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/state_dumper.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/state_table.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/statespace.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tree_compress.cpp
)

target_compile_definitions(lmn_verifier PUBLIC FMT_HEADER_ONLY)

target_link_libraries(lmn_verifier PRIVATE zd_in_slim)
target_link_libraries(lmn_verifier PRIVATE ZLIB::ZLIB)

execute_process(
    COMMAND
    ${RE2C_EXECUTABLE} -o ${NC_LEXER_OUTPUT} ${NC_LEXER_INPUT}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

execute_process(
    COMMAND
    ${RE2C_EXECUTABLE} -c -o ${PROPSYM_LEXER_OUTPUT} ${PROPSYM_LEXER_INPUT}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

set_property(
  TARGET lmn_verifier
  APPEND
  PROPERTY ADDITIONAL_CLEAN_FILES ${PROPSYM_LEXER_OUTPUT} ${NC_LEXER_OUTPUT} ${NC_PARSER_OUTPUT} ${NC_PARSER_DEF} ${PROPSYM_PARSER_OUTPUT} ${PROPSYM_PARSER_DEF}
)