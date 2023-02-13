/*
 * main.cpp
 *
 *   Copyright (c) 2019, Ueda Laboratory LMNtal Group
 * <lmntal@ueda.info.waseda.ac.jp> All rights reserved.
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions are
 *   met:
 *
 *    1. Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *    2. Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in
 *       the documentation and/or other materials provided with the
 *       distribution.
 *
 *    3. Neither the name of the Ueda Laboratory LMNtal Group nor the
 *       names of its contributors may be used to endorse or promote
 *       products derived from this software without specific prior
 *       written permission.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 *   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 *   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 *   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 *   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "../element/element.h"

#include "check_result.hpp"
#include "identity.hpp"
#include "isomorphism.hpp"
#include "state_space.hpp"

#include <functional>
#include <getopt.h>
#include <iostream>
#include <istream>

#ifndef COMMIT_ID
#define COMMIT_ID ""
#endif

// dummy
void slim_version(FILE *f) {}

namespace ssc {

std::istream *input1 = nullptr;
std::istream *input2 = nullptr;

std::function<bool(const json_t &, const json_t &)> state_comparator =
    [](const json_t &, const json_t &) { return true; };

enum class EquivalenceMethod { identity, isomorphism };
EquivalenceMethod equivalence_method = EquivalenceMethod::identity;

enum PrintLevel { quiet = 0, info, verbose, debug };
PrintLevel print_level = PrintLevel::info;

static void usage(void) {
  fprintf(
      stderr,
      "Usage: sscheck [OPTION]... FILE1 FILE2\n"
      "options:\n"
      "  --stdin              Read stndard input instead of FILE1.\n"
      "  -e, --equivalence=(identity | isomorphism)\n"
      "                       Set how to decide equivalence of state spaces.\n"
      "                       Default is identity."
      "  -c, --compare=(nothing | string)\n"
      "                       Set how to decide equivalence of states.\n"
      "                       Default is nothing (don't care what state is).\n"
      "  --quiet              Print no information.\n"
      "  --info               Print human-readable results (default).\n"
      "  --verbose            Print more information.\n"
      "  -h, --help           Print this message.\n"
      "  -v, --version        Show version.\n");
  exit(1);
}

static void parse_options(int argc, char *argv[]) {
  int c, option_index;

  struct option long_options[] = {
      {"version", no_argument, nullptr, 1000},
      {"help", no_argument, nullptr, 1001},
      {"stdin", no_argument, nullptr, 2000},
      {"equivalence", required_argument, nullptr, 3000},
      {"compare", required_argument, nullptr, 3001},
      {"quiet", no_argument, nullptr, 4000},
      {"info", no_argument, nullptr, 4001},
      {"verbose", no_argument, nullptr, 4002},
      {"debug", no_argument, nullptr, 4003},
      {0, 0, 0, 0}};

  while ((c = getopt_long(argc, argv, "+vhe:c:", long_options, &option_index)) !=
         -1) {
    switch (c) {
    case 0:
      printf("sscheck: log_options entries must have positive 4th member.\n");
      exit(1);
      break;

    case 'v':
    case 1000:
      std::cout << "sscheck (State Space Checker) - version " << COMMIT_ID << std::endl;
      exit(1);
      break;

    case 'h':
    case 1001: /* help */ /* FALLTHROUGH */
    case '?':
      usage();
      break;

    case 2000:
      input1 = &std::cin;
      break;

    case 'e':
    case 3000:
      if (!optarg)
        break;
      else if (strcmp(optarg, "identity") == 0)
        equivalence_method = EquivalenceMethod::identity;
      else if (strcmp(optarg, "isomorphism") == 0)
        equivalence_method = EquivalenceMethod::isomorphism;
      break;

    case 'c':
    case 3001:
      if (!optarg)
        break;
      else if (strcmp(optarg, "nothing") == 0)
        state_comparator = [](const json_t &, const json_t &) { return true; };
      else if (strcmp(optarg, "string") == 0)
        state_comparator = [](const json_t &a, const json_t &b) {
          return a == b;
        };
      break;

    case 4000:
      print_level = PrintLevel::quiet;
      break;

    case 4001:
      print_level = PrintLevel::info;
      break;

    case 4002:
      print_level = PrintLevel::verbose;
      break;

    case 4003:
      print_level = PrintLevel::debug;
      break;

    default:
      printf("?? getopt returned character code 0x%x ??\n", c);
      exit(1);
      break;
    }
  }
}

} // namespace ssc

using namespace ssc;

int main(int argc, char *argv[]) {
  if (argc == 1) {
    usage();
    exit(1);
  }

  parse_options(argc, argv);

  int index = optind;
  while (index < argc) {
    auto input = new std::fstream(argv[index++]);
    if (!input1) {
      input1 = input;
    } else if (!input2) {
      input2 = input;
    } else {
      std::cerr << "sscheck: too much inputs" << std::endl;
      exit(1);
    }
  }

  if (!input1 || !input2) {
    std::cerr << "sscheck: missing state spaces" << std::endl;
    exit(1);
  }

  slim::element::json_t json1, json2;
  try {
    (*input1) >> json1;
    if (input1 != &std::cin)
      delete input1;
    (*input2) >> json2;
    if (input2 != &std::cin)
      delete input2;
  } catch (slim::element::json::parse_error &error) {
    std::cerr << "sscheck: " << error.what() << std::endl;
    exit(1);
  }

  check_result result;
  switch (equivalence_method) {
  case EquivalenceMethod::identity:
    result = identity_check(json1, json2, state_comparator);
    break;
  case EquivalenceMethod::isomorphism:
    result = isomorphism_check(json1, json2, state_comparator);
    break;
  }

  if (print_level >= PrintLevel::info) {
    std::cout << "sscheck: " << (result ? "equivalent" : "different")
              << std::endl;
  }

  if (print_level >= PrintLevel::verbose) {
    if (result) {
      // result.morphismを出力する
    } else {
      std::cout << result.reason << std::endl;
    }
  }

  return (result) ? 0 : 1;
}