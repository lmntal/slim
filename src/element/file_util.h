/*
 * file_util.h - file utilities
 *
 *   Copyright (c) 2008, Ueda Laboratory LMNtal Group
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
 *
 * $Id$
 */

#ifndef LMN_FILE_UTIL_H
#define LMN_FILE_UTIL_H

/**
 * @ingroup Element
 * @defgroup FileUtil
 * @{
 */

/* deprecated functions */
char *build_path(char const *dir, char const *component);
char *basename_ext(char const *path);
char *extension(char const *path);

#include <dirent.h>
#include <sys/stat.h>

#include <cstring>
#include <iterator>
#include <memory>
#include <string>
#include <string_view>
#include <system_error>
#include <utility>

#include "arch.h"

namespace slim::element::filesystem {
class path {
  std::string str_;

public:
  path() = default;
  path(std::string src) : str_(std::move(src)) {}

  path filename() const {
    auto pos = str_.find_last_of(DIR_SEPARATOR_CHAR);
    return (pos == std::string::npos) ? *this : str_.substr(pos + 1, str_.size() - pos - 1);
  }
  path stem() const { return str_.substr(0, str_.find_last_of('.')); }
  path extension() const {
    auto pos = str_.find_last_of('.');
    return (pos == std::string::npos) ? "" : str_.substr(pos + 1, str_.size() - pos - 1);
  }
  path &replace_extension(path const &replacement = path()) {
    str_ = str_.substr(0, str_.find_last_of('.')) + '.' + replacement.str_;
    return *this;
  }
  std::string string() const { return str_; }
};
inline path operator/(path const &lhs, path const &rhs) {
  if (lhs.string().back() == DIR_SEPARATOR_CHAR) {
    return lhs.string() + rhs.string();
  }
  return lhs.string() + DIR_SEPARATOR_CHAR + rhs.string();
}

class filesystem_error : public std::system_error {
  using std::system_error::system_error;
};

using file_status = struct stat;

class directory_entry {
  struct dirent    ent_;
  filesystem::path path_;
  bool             emp;

public:
  directory_entry() : emp(true) {}
  directory_entry(dirent ent, filesystem::path dirpath) : ent_(ent), emp(false) {
    path_ = dirpath / std::string(ent_.d_name, strlen(ent_.d_name));
  }

  filesystem::path const &path() const { return path_; }
                          operator filesystem::path const &() const { return path_; }

  file_status status() const {
    struct stat st;
    if (stat(path_.string().c_str(), &st) == -1) {
      throw filesystem_error(std::error_code(errno, std::system_category()), "");
    }
    return st;
  }

  bool empty() const { return emp; }
};

class directory_stream {
  filesystem::path dirpath;
  DIR             *dir;

public:
  directory_stream(std::string const &name) : dirpath(name) {
    dir = opendir(name.c_str());
    if (dir == nullptr) {
      throw filesystem_error(std::error_code(errno, std::system_category()), name);
    }
  }

  ~directory_stream() noexcept {
    if (closedir(dir) != 0) {
      // throw filesystem_error(std::error_code(errno, std::system_category()), dirpath.string());
    }
  }

  [[nodiscard]] long loc() const { return telldir(dir); }

  directory_entry read() {
    auto  err = errno;
    auto *e   = readdir(dir);
    if (e == nullptr && err != errno) {
      throw filesystem_error(std::error_code(errno, std::system_category()), dirpath.string());
    }
    return (e == nullptr) ? directory_entry() : directory_entry(*e, dirpath);
  }
  void seek(long loc) { seekdir(dir, loc); }
};

class directory_iterator {
  std::shared_ptr<directory_stream> dir;
  long                              loc;
  long                              next_loc;
  directory_entry                   ent;

public:
  using value_type        = directory_entry;
  using difference_type   = std::ptrdiff_t;
  using pointer           = directory_entry const *;
  using reference         = directory_entry const &;
  using iterator_category = std::input_iterator_tag;

  directory_iterator() : dir(nullptr), loc(-1) {}
  directory_iterator(path const &p) : dir(std::make_shared<directory_stream>(p.string())) {
    loc      = dir->loc();
    ent      = dir->read();
    next_loc = dir->loc();
  }

  directory_iterator(directory_iterator const &) = default;
  directory_iterator(directory_iterator &&)      = default;

  directory_iterator &operator=(directory_iterator const &) = default;
  directory_iterator &operator=(directory_iterator &&)      = default;

  directory_entry const &operator*() const { return ent; }
  directory_entry const *operator->() const { return &ent; }

  directory_iterator &operator++() {
    if (ent.empty())
      return *this;

    dir->seek(next_loc);
    ent      = dir->read();
    loc      = next_loc;
    next_loc = dir->loc();
    return *this;
  }

  bool operator==(directory_iterator const &it) const {
    return (ent.empty() && ent.empty() == it.ent.empty()) || (dir == it.dir && loc == it.loc);
  }
  bool operator!=(directory_iterator const &it) const { return !(*this == it); };
};

inline directory_iterator begin(directory_iterator iter) { return iter; }
inline directory_iterator end(directory_iterator const &) { return directory_iterator(); }

inline bool is_regular_file(file_status s) { return S_ISREG(s.st_mode); }
inline bool exists(filesystem::path const &p) {
  struct stat st;
  return stat(p.string().c_str(), &st) != 0;
}

} // namespace slim::element::filesystem

#endif /* !LMN_UTIL_H */
