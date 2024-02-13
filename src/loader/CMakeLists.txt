add_library(lmn_loader STATIC)

set(LEXER_OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/il_lexer.cpp)
set(LEXER_INPUT ${CMAKE_CURRENT_SOURCE_DIR}/il_lexer.cpp.re)
set(PARSER_OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/il_parser.cpp)
set(PARSER_DEF ${CMAKE_CURRENT_SOURCE_DIR}/il_parser.hpp)

set(TRANSLATION_GENERATOR ${CMAKE_CURRENT_SOURCE_DIR}/translate_generator.rb)
set(TRANSLATION_INPUT ${CMAKE_CURRENT_SOURCE_DIR}/translate_generator.in)
set(TRANSLATION_OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/translate_generated.cpp)
set(INTERPRETATION_OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/interpret_generated.cpp)

bison_target(
    PARSER ${CMAKE_CURRENT_SOURCE_DIR}/il_parser.ypp ${PARSER_OUTPUT}
    DEFINES_FILE ${PARSER_DEF}
    COMPILE_FLAGS "-d -p il"
)

target_include_directories(lmn_loader PRIVATE
    ${CMAKE_SOURCE_DIR}/src
)

target_sources(lmn_loader PRIVATE
    ${LEXER_OUTPUT}
    ${PARSER_OUTPUT}
)

target_sources(lmn_loader PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/byte_encoder.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/load.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/loader.c
    ${CMAKE_CURRENT_SOURCE_DIR}/syntax.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/translate.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/translate_generated.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/interpret_generated.cpp
    ${LEXER_OUTPUT}
    ${PARSER_OUTPUT}
)

target_link_libraries(lmn_loader PRIVATE
    lmn_elm lmn_ffi
)

execute_process(
    COMMAND
    ${RE2C_EXECUTABLE} -o ${LEXER_OUTPUT} ${LEXER_INPUT}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

execute_process(
    COMMAND
    ${Ruby_EXECUTABLE} --encoding=utf-8 -i ${TRANSLATION_GENERATOR}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    INPUT_FILE ${TRANSLATION_INPUT}
    OUTPUT_FILE ${INTERPRETATION_OUTPUT}
)

execute_process(
    COMMAND
    ${Ruby_EXECUTABLE} --encoding=utf-8 ${TRANSLATION_GENERATOR}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    INPUT_FILE ${TRANSLATION_INPUT}
    OUTPUT_FILE ${TRANSLATION_OUTPUT}
)

set_property(
  TARGET lmn_loader
  APPEND
  PROPERTY ADDITIONAL_CLEAN_FILES ${LEXER_OUTPUT} ${PARSER_OUTPUT} ${PARSER_DEF} ${TRANSLATION_OUTPUT} ${INTERPRETATION_OUTPUT}
)