/*
 * binstr.cpp
 *
 *   Copyright (c) 2018, Ueda Laboratory LMNtal Group
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

#include "binstr.hpp"

bool BinStrCursor::push(const BYTE *v, int size) {
  const int half_len = size / 2;
  if (!is_valid())
    return false;

  if (direct) {
    if (size & 1) {
      binstr->write_direct(v[size >> 1] & 0x0f, pos_++);
    }
    for (int i = half_len - 1; i >= 0; i--) {
      binstr->write_direct((v[i] >> TAG_BIT_SIZE & 0x0f), pos_++);
      binstr->write_direct(v[i] & 0x0f, pos_++);
    }
    return true;
  } else {
    if (size & 1) {
      if (binstr->write(v[size >> 1] & 0x0f, pos_))
        pos_++;
      else {
        invalidate();
        return false;
      }
    }

    for (int i = half_len - 1; i >= 0; i--) {
      if (binstr->write(v[i] >> TAG_BIT_SIZE & 0x0f, pos_)) {
        pos_++;
      } else {
        invalidate();
        return false;
      }
      if (binstr->write(v[i] & 0x0f, pos_))
        pos_++;
      else {
        invalidate();
        return false;
      }
    }
    return true;
  }
}

/* pのBinStrのバイト列へ4bitのTAG vを書き込む. */
bool BinStrCursor::push(BYTE v) {
  if (!is_valid())
    return false;

  if (binstr->write(v & 0x0f, pos_)) {
    pos_++;
    return true;
  } else {
    invalidate();
    return false;
  }
}
