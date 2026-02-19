add_library(lmn_vm STATIC
    vm.cpp
    membrane.cpp membrane.h
    membrane.hpp
    atomlist.hpp
    atom.cpp atom.h
    ccallback.cpp ccallback.h
    dumper.cpp dumper.h
    functor.cpp functor.h
    firstclass_rule.cpp firstclass_rule.h
    hyperlink.cpp hyperlink.h
    normal_thread.cpp normal_thread.h
    react_context.cpp react_context.hpp
    rule.cpp rule.hpp
    special_atom.cpp special_atom.h
    symbol.cpp symbol.h
    symbol_atom_range.hpp
    system_ruleset.cpp system_ruleset.h
    task.cpp task.h
    process_table.cpp process_table.h process_table.hpp
    simply_process_table.cpp simply_process_table.h
    trace_log.h trace_log.cpp
    simply_trace_log.h simply_trace_log.cpp
    interactive_debug.hpp interactive_debug.cpp
    debug_printer.hpp debug_printer.cpp
)

target_include_directories(lmn_vm PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/..
    ${CMAKE_CURRENT_SOURCE_DIR}/../../third_party/zdelta-2.1
    ${CMAKE_BINARY_DIR}/src
)

target_link_libraries(lmn_vm PRIVATE
    Threads::Threads
)

if(OpenMP_FOUND)
    target_link_libraries(lmn_vm PRIVATE OpenMP::OpenMP_CXX)
endif()