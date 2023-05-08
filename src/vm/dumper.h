/*
 * dumper.h
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
 * $Id: dumper.h,v 1.3 2008/09/19 05:18:17 taisuke Exp $
 */

#ifndef LMN_DUMPER_H
#define LMN_DUMPER_H

/**
 * @ingroup VM
 * @defgroup Dumper
 * @{
 */

#include "element/element.h"
#include "lmntal.h"
#include "membrane.h"
#include "rule.h"

#include <ostream>
#include <string>

/**
 * @brief initialize dumper module.
 *
 * \e dumper_init must be called just once before use.
 * This function just register callback so far.
 */
LMN_EXTERN void dumper_init();
/**
 * @brief finalize dumper module.
 *
 * \e dumper_finalize muste be called just once after use.
 * This function does nothing.
 */
LMN_EXTERN void dumper_finalize();
/**
 * @brief print a membrane to stdout.
 */
LMN_EXTERN void lmn_dump_mem_stdout(LmnMembraneRef mem);
/**
 * @brief print a membrane.
 */
LMN_EXTERN void lmn_dump_mem(LmnMembraneRef mem, LmnPortRef port);
/**
 * @brief print a membrane in development mode.
 */
LMN_EXTERN void lmn_dump_mem_dev(LmnMembraneRef mem);
/**
 * @brief print the contents of a membrane to stdout.
 */
LMN_EXTERN void lmn_dump_cell_stdout(LmnMembraneRef mem);
/**
 * @brief print the contents of a membrane.
 */
LMN_EXTERN void lmn_dump_cell(LmnMembraneRef mem, LmnPortRef port);
/**
 * @brief print a membrane in dot format.
 */
LMN_EXTERN void lmn_dump_dot(LmnMembraneRef mem);
/**
 * @brief print the instructions of a rule.
 * @deprecated
 */
LMN_EXTERN void lmn_dump_rule(LmnPortRef port, LmnRuleSetRef rs);
/**
 * @brief print rules.
 * @deprecated
 */
LMN_EXTERN void lmn_dump_ruleset(LmnPortRef port, std::vector<LmnRuleSet *> const &v);
/**
 * @brief print an atom and its connected ones.
 */
LMN_EXTERN void lmn_dump_atom(LmnPortRef port, LmnAtomRef atom, LmnLinkAttr attr);

/**
 * @brief print an escaped string.
 */
void dump_escaped(LmnPortRef port, char const *s);

extern char char_to_escape_char[];

namespace slim {
namespace format {
std::ostream &env(std::ostream &os);
std::ostream &lmntal(std::ostream &os);
std::ostream &verbal(std::ostream &os);
std::ostream &dot(std::ostream &os);
std::ostream &json(std::ostream &os);
} // namespace format

std::string               to_string(LmnMembrane const *mem);
static inline std::string to_string_membrane(LmnMembrane const *mem) {
  return (lmn_env.output_format == OutputFormat::DEFAULT) ? ("{" + to_string(mem) + "}") : to_string(mem);
}

void dump_mem(std::ostream &os, LmnMembrane const *mem);
} // namespace slim

static inline std::ostream &operator<<(std::ostream &os, LmnMembrane const &mem) {
  slim::dump_mem(os, &mem);
  return os;
}

/* @} */

#endif /* LMN_DUMPER_H */
