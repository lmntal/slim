# Custom commands for parser and code generation
add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/il_parser.cpp ${CMAKE_CURRENT_BINARY_DIR}/il_parser.hpp
    COMMAND ${BISON_EXECUTABLE} -d -p il -o ${CMAKE_CURRENT_BINARY_DIR}/il_parser.cpp ${CMAKE_CURRENT_SOURCE_DIR}/il_parser.ypp
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/il_parser.ypp
    COMMENT "Generating il_parser.cpp"
)

add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/il_lexer.cpp
    COMMAND ${RE2C_EXECUTABLE} -o ${CMAKE_CURRENT_BINARY_DIR}/il_lexer.cpp ${CMAKE_CURRENT_SOURCE_DIR}/il_lexer.cpp.re
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/il_lexer.cpp.re
    COMMENT "Generating il_lexer.cpp"
)

add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/translate_generated.cpp
    COMMAND ${RUBY_EXECUTABLE} --encoding=utf-8 ${CMAKE_CURRENT_SOURCE_DIR}/translate_generator.rb < ${CMAKE_CURRENT_SOURCE_DIR}/translate_generator.in > ${CMAKE_CURRENT_BINARY_DIR}/translate_generated.cpp
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/translate_generator.in ${CMAKE_CURRENT_SOURCE_DIR}/translate_generator.rb
    COMMENT "Generating translate_generated.cpp"
)

add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/interpret_generated.cpp
    COMMAND ${RUBY_EXECUTABLE} --encoding=utf-8 ${CMAKE_CURRENT_SOURCE_DIR}/translate_generator.rb -i < ${CMAKE_CURRENT_SOURCE_DIR}/translate_generator.in > ${CMAKE_CURRENT_BINARY_DIR}/interpret_generated.cpp
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/translate_generator.in ${CMAKE_CURRENT_SOURCE_DIR}/translate_generator.rb
    COMMENT "Generating interpret_generated.cpp"
)

add_library(lmn_loader STATIC
    loader.h
    load.cpp load.h
    ${CMAKE_CURRENT_BINARY_DIR}/il_parser.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/il_lexer.cpp
    syntax.cpp syntax.hpp
    byte_encoder.cpp byte_encoder.hpp
    translate.cpp translate.hpp
    ${CMAKE_CURRENT_BINARY_DIR}/translate_generated.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/interpret_generated.cpp
    so.h
    exception.hpp
)

target_include_directories(lmn_loader PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/..
    ${CMAKE_CURRENT_SOURCE_DIR}/../../third_party/zdelta-2.1
    ${CMAKE_CURRENT_BINARY_DIR}
    ${CMAKE_BINARY_DIR}/src
)

target_link_libraries(lmn_loader PRIVATE
    Threads::Threads
)

if(OpenMP_FOUND)
    target_link_libraries(lmn_loader PRIVATE OpenMP::OpenMP_CXX)
endif()