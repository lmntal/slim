/*
 * firstclass_rule.h
 *
 *   Copyright (c) 2017, Ueda Laboratory LMNtal Group
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

#ifndef LMN_FIRSTCLASS_RULE_H
#define LMN_FIRSTCLASS_RULE_H

#include "atom.h"
#include "lmntal.h"
#include "membrane.h"

/**
 * @ingroup VM
 * @defgroup FirstClassRule
 * @{
 */

/**
 * @brief Initialize a table to associate first-class rulesets with
 * <tt>':-'/3</tt> atoms.
 *
 * @note
 *     This function must be called exactly once before using functions of
 * first-class rulesets.
 */
void first_class_rule_tbl_init();

/**
 * @brief Create a first-class ruleset from a <tt>':-'/3</tt> atom.
 * @return A ruleset or @c NULL.
 *
 * @note
 *     The caller must free the returned ruleset unless it's @c NULL.
 */
LmnRuleSetRef firstclass_ruleset_create(LmnSymbolAtomRef imply);

/**
 * @brief Remove a first-class ruleset associated with @c imply from the table.
 *
 * @note
 *     @c imply must be associated with a first-class ruleset.
 */
void firstclass_ruleset_release(LmnSymbolAtomRef imply);

/**
 * @brief Get a first-class ruleset associated with @c imply.
 * @return A ruleset, or if no rule is associated with @c imply it returns @c
 * NULL.
 */
LmnRuleSetRef firstclass_ruleset_lookup(LmnSymbolAtomRef imply);

/** @} */

#endif /* LMN_FIRSTCLASS_RULE_H */
