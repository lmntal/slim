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

#ifndef LMN_STRINGIFIER_H
#define LMN_STRINGIFIER_H

#include "element/element.h"
#include "lmntal.h"
#include "membrane.h"
#include "rule.h"

namespace slim {
namespace stringifier {
/**
 * @brief return string representation of a membrane.
 */
std::string lmn_stringify_mem(LmnMembraneRef mem);
/**
 * @brief return string representation of a membrane in development mode.
 */
std::string lmn_stringify_mem_dev(LmnMembraneRef mem);
/**
 * @brief return string representation of the contents of a membrane.
 */
std::string lmn_stringify_cell(LmnMembraneRef mem);
/**
 * @brief return string representation of a membrane in dot format.
 */
std::string lmn_stringify_dot(LmnMembraneRef mem);
/**
 * @brief return string representation of the instructions of a rule.
 * @deprecated
 */
std::string lmn_stringify_rule(LmnRuleSetRef rs);
/**
 * @brief return string representation of rules.
 * @deprecated
 */
std::string lmn_stringify_ruleset(const std::vector<LmnRuleSet *> &v);
/**
 * @brief return string representation of an atom and its connected ones.
 */
std::string lmn_stringify_atom(LmnAtomRef atom, LmnLinkAttr attr);

/**
 * @brief return string representation of an escaped string.
 */
std::string stringify_escaped(const char *s);

// additional functions
std::string stringify_atom_dev(LmnSymbolAtomRef atom);
}
}

#endif /* LMN_STRINGIFIER_H */
