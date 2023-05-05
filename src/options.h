#pragma once

#include <cxxopts/cxxopts.hpp>

extern cxxopts::Options slim_options;

enum class CommandLineParseResult {
  OK,   // used when parsing is successful
  EXIT, // used when --help or --version is passed
  ERROR // used when there is an error in parsing
};

void init_options();
auto parse_command_line(cxxopts::ParseResult const& result) -> CommandLineParseResult;