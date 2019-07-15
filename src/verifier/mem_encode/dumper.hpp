/*
 * dumper.hpp
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
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
 *
 */

#ifndef SLIM_VERIFIER_MEM_ENCODE_DUMPER_HPP
#define SLIM_VERIFIER_MEM_ENCODE_DUMPER_HPP

#include "lmntal.h"

#include "decoder.hpp"
#include "halfbyte_scanner.hpp"

namespace slim {
namespace verifier {
namespace mem_encode {
struct dumper {
  void dump_data_atom(halfbyte_scanner &scanner, unsigned int tag) {
    switch (tag) {
    case TAG_INT_DATA: {
      long n = scanner.scan_integer();
      printf("_INT%ld_ ", n);
    } break;
    case TAG_DBL_DATA: {
      auto n = scanner.scan_double();
      printf("_DBL%lf_", n);
    } break;
    case TAG_SP_ATOM_DATA: {
      auto type = scanner.scan_sp_atom_type();
      auto bytes = scanner.scan_bytes();
      auto atom = sp_atom_decoder(type)(bytes);
      SP_ATOM_DUMP(atom, lmn_stdout_port());
    } break;
    default:
      lmn_fatal("unexpected.");
      break;
    }
  }

  void dump(BYTE *bs, int len) {
    std::vector<BsDecodeLog> log(len * TAG_IN_BYTE);
    halfbyte_scanner scanner(bs, len);
    auto v_i = 1;
    while (scanner.location() < len) {
      unsigned int tag = scanner.scan_tag();

      switch (tag) {
      case TAG_ATOM_START: {
        LmnFunctor f = scanner.scan_functor();
        log[v_i] = {(LmnWord)f, BS_LOG_TYPE_ATOM};
        printf("%s/%d_%d ", lmn_id_to_name(LMN_FUNCTOR_NAME_ID(lmn_functor_table, f)),
               LMN_FUNCTOR_ARITY(lmn_functor_table, f), v_i++);
      } break;
      case TAG_NAMED_MEM_START: /* FALL THROUGH */
      case TAG_MEM_START: {
        log[v_i] = {0, BS_LOG_TYPE_MEM}; /* とりえあずゼロクリア */

        if (tag == TAG_MEM_START) {
          printf("_%d{ ", v_i);
        } else {
          lmn_interned_str name = scanner.scan_mem_name();
          printf("%s_%d{ ", lmn_id_to_name(name), v_i);
        }

        v_i++;
      } break;
      case TAG_MEM_END: {
        printf("}. ");
      } break;
      case TAG_HLINK: {
        LmnHlinkRank hl_num;

        hl_num = scanner.scan_ref_num();

        log[v_i] = {0, BS_LOG_TYPE_HLINK}; /* とりあえずゼロクリア */
        printf("HL/%d_%d ", hl_num, v_i++);

        tag = scanner.scan_tag();
        switch (tag) {
        case TAG_FROM: {
          printf("_F_ ");
        } break;
        case TAG_ATOM_START: {
          LmnFunctor f = scanner.scan_functor();
          printf("%s/%d ", lmn_id_to_name(LMN_FUNCTOR_NAME_ID(lmn_functor_table, f)),
                 LMN_FUNCTOR_ARITY(lmn_functor_table, f));
        } break;
        case TAG_INT_DATA:
        case TAG_DBL_DATA:
        case TAG_SP_ATOM_DATA: {
          dump_data_atom(scanner, tag);
        } break;
        default:
          lmn_fatal("unexpected");
          break;
        }
      } break;
      case TAG_VISITED_ATOMHLINK: /* FALL THROUGH */
      case TAG_VISITED_MEM: {
        unsigned int ref = scanner.scan_ref_num();

        switch (log[ref].type) {
        case BS_LOG_TYPE_ATOM:
          printf("$%d's%d ", ref, scanner.scan_arg_ref());
          break;
        case BS_LOG_TYPE_MEM:
          printf("#%d ", ref);
          break;
        case BS_LOG_TYPE_HLINK:
          printf("$HL%d ", ref);
          break;
        default:
          lmn_fatal("unexpected reference");
          break;
        }
      } break;
      case TAG_ESCAPE_MEM_DATA: {
        unsigned int sub_tag = scanner.scan_tag();
        printf("!");
        dump_data_atom(scanner, sub_tag);
        printf("! ");
      } break;
      case TAG_ESCAPE_MEM: {
        printf("! ");
      } break;
      case TAG_FROM: {
        printf("_F_ ");
      } break;
      case TAG_INT_DATA: /* FALL TROUGH */
      case TAG_DBL_DATA: /* FALL TROUGH */
      case TAG_SP_ATOM_DATA:
        dump_data_atom(scanner, tag);
        break;
      case TAG_RULESET1: {
        int rs_id = scanner.scan_ruleset();
        printf("@%d", rs_id);
      } break;
      case TAG_RULESET: {
        int j, n, rs_id;

        n = scanner.scan_ruleset_num();
        for (j = 0; j < n; j++) {
          rs_id = scanner.scan_ruleset();
          printf("@%d", rs_id);
        }
      } break;
      case TAG_RULESET_UNIQ: {
        LmnRuleSetRef rs;
        lmn_interned_str id;
        unsigned int j, k, l, n, rs_id, rule_num, his_num;

        n = scanner.scan_ruleset_num();
        for (j = 0; j < n; j++) {
          rs_id = scanner.scan_ruleset();
          printf("@%d/", rs_id);

          /* dump applied histories of uniq constraint rules */

          rs = LmnRuleSetTable::at(rs_id);
          rule_num = rs->size();

          for (k = 0; k < rule_num; k++) {
            printf("[%s", lmn_id_to_name(rs->get_rule(k)->name));

            his_num = scanner.scan_history_num();
            for (l = 0; l < his_num; l++) {
              id = scanner.scan_history();
              printf("\"%s\"", lmn_id_to_name(id));
            }
            printf("]");
          }
        }
      } break;
      default:
        printf("pos = %lu, len = %d\n", scanner.location(), len);
        lmn_fatal("unexpected");
        break;
      }
    }
    printf("\n");
  }

  static void binstr_dump(BYTE *bs, int len) {
    dumper d;
    d.dump(bs, len);
  }
};

} // namespace mem_encode
} // namespace verifier
} // namespace slim

#endif /* SLIM_VERIFIER_MEM_ENCODE_DUMPER_HPP */
