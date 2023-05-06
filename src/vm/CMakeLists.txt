add_library(lmn_vm STATIC)

target_include_directories(lmn_vm PRIVATE
    ${CMAKE_SOURCE_DIR}/src
)

target_sources(lmn_vm PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/atom.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/ccallback.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/debug_printer.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/dumper.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/firstclass_rule.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/functor.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/hyperlink.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/interactive_debug.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/membrane.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/normal_thread.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/process_table.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/react_context.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/rule.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/simply_process_table.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/simply_trace_log.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/special_atom.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/symbol.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/system_ruleset.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/task.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/trace_log.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/vm.cpp
)

target_compile_definitions(lmn_vm PUBLIC FMT_HEADER_ONLY)