/*
 * stack_trace.h
 *
 *   Copyright (c) 2018, Ueda Laboratory LMNtal Group
 *                                          <lmntal@ueda.info.waseda.ac.jp>
 *   All rights reserved.
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

#ifndef SLIM_ELEMENT_STACK_TRACE_H
#define SLIM_ELEMENT_STACK_TRACE_H

#include <vector>
#include <string>

namespace slim {
namespace element {
namespace stack_trace {

#if defined(HAVE_EXECINFO_H) && HAVE_EXECINFO_H

#include <execinfo.h>

static constexpr size_t size = 32;

inline std::vector<std::string> backtrace() {
	void *addrs[size];
	auto num_traces = backtrace(addrs, size);
	auto symbols = backtrace_symbols(addrs, num_traces);
	std::vector<std::string> res(num_traces);
	for (int i = 0; i < num_traces; i++) {
		res[i] = symbols[i];
	}
	free(symbols);
	return res;
}

#else

inline std::vector<std::string> backtrace() {
	return std::vector<std::string>({"can't create backtrace."});
}

#endif

}
}
}

#endif /* SLIM_ELEMENT_STACK_TRACE_H */
