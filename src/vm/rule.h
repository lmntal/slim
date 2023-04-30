/*
 * rule.h - types and functions about rule, rule set, module
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
 * $Id: rule.h,v 1.6 2008/09/29 05:23:40 taisuke Exp $
 */

#ifndef LMN_RULE_H
#define LMN_RULE_H

/**
 * @ingroup VM
 * @defgroup Rule
 * @{
 */
struct LmnRule;
struct LmnRuleSet;
using LmnRuleRef = LmnRule*;
using LmnRuleSetRef = LmnRuleSet*;

#include "element/element.h"
#include "lmntal.h"
#include "symbol.h"

#include <vector>

/*----------------------------------------------------------------------
 * Rule Set
 */

bool lmn_rulesets_equals(const std::vector<LmnRuleSetRef> &rulesets1, const std::vector<LmnRuleSetRef> &rulesets2);

/*----------------------------------------------------------------------
 * System Rule Set
 */

extern LmnRuleSetRef system_ruleset;
void lmn_add_system_rule(LmnRuleRef rule);

/*----------------------------------------------------------------------
 * Initial Rule Set
 */

extern LmnRuleSetRef initial_ruleset;
extern LmnRuleSetRef initial_system_ruleset;
void lmn_add_initial_rule(LmnRuleRef rule);
void lmn_add_initial_system_rule(LmnRuleRef rule);

/*----------------------------------------------------------------------
 * Module
 */

LMN_EXTERN void lmn_set_module(lmn_interned_str module_name,
                               LmnRuleSetRef ruleset);
LMN_EXTERN LmnRuleSetRef lmn_get_module_ruleset(lmn_interned_str module_name);

/* @} */

#endif /* LMN_RULE_H */
