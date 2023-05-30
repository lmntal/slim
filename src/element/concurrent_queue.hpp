/*
 *   concurrent_queue.hpp
 *
 *   Copyright (c) 2023, Ueda Laboratory LMNtal Group
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
 *   qrwells
 */

#pragma once

#include <mutex>
#include <queue>

/*
 * @brief Queue interface
 * @tparam T type of value
 */
template <typename T> class CQueue {
public:
  using value_type = T;

  virtual ~CQueue() = default;

  virtual void enqueue(T const &v)   = 0;
  virtual bool try_dequeue(T &value) = 0;
  virtual T    dequeue()             = 0;
  virtual void clear()               = 0;

  inline size_t size() const { return m_queue.size(); }
  inline bool   empty() const { return m_queue.empty(); }

protected:
  std::queue<T> m_queue;
};

template <typename T> class SPSCQueue final : public CQueue<T> {
  using CQueue<T>::m_queue;

public:
  SPSCQueue()  = default;
  ~SPSCQueue() = default;

  inline void enqueue(T const &v) override { m_queue.push(v); }

  inline bool try_dequeue(T &value) override {
    if (m_queue.empty()) {
      return false;
    }
    value = m_queue.front();
    m_queue.pop();
    return true;
  }

  inline T dequeue() override {
    T value{};
    if (m_queue.empty()) {
      return value;
    }
    value = m_queue.front();
    m_queue.pop();
    return value;
  }

  inline void clear() override {
    std::queue<T> empty;
    std::swap(m_queue, empty);
  }
};

template <typename T> class MPSCQueue final : public CQueue<T> {
  using CQueue<T>::m_queue;

public:
  inline void enqueue(T const &v) override {
    std::lock_guard<std::mutex> lock(m_mutex);
    m_queue.push(v);
  }

  inline bool try_dequeue(T &value) override {
    if (m_queue.empty()) {
      return false;
    }
    value = m_queue.front();
    m_queue.pop();
    return true;
  }

  inline T dequeue() override {
    T value{};
    if (m_queue.empty()) {
      return value;
    }
    value = m_queue.front();
    m_queue.pop();
    return value;
  }

  inline void clear() override {
    std::lock_guard<std::mutex> lock(m_mutex);
    std::queue<T>               empty;
    std::swap(m_queue, empty);
  }

private:
  std::mutex m_mutex;
};

template <typename T> class SPMCQueue final : public CQueue<T> {
  using CQueue<T>::m_queue;

public:
  inline void enqueue(T const &v) override { m_queue.push(v); }

  inline bool try_dequeue(T &value) override {
    std::lock_guard<std::mutex> lock(m_mutex);
    if (m_queue.empty()) {
      return false;
    }
    value = m_queue.front();
    m_queue.pop();
    return true;
  }

  inline T dequeue() override {
    std::lock_guard<std::mutex> lock(m_mutex);
    T                           value{};
    if (m_queue.empty()) {
      return value;
    }
    value = m_queue.front();
    m_queue.pop();
    return value;
  }

  inline void clear() override {
    std::lock_guard<std::mutex> lock(m_mutex);
    std::queue<T>               empty;
    std::swap(m_queue, empty);
  }

private:
  std::mutex m_mutex;
};

template <typename T> class MPMCQueue final : public CQueue<T> {
  using CQueue<T>::m_queue;

public:
  inline void enqueue(T const &v) override {
    std::lock_guard<std::mutex> lock(m_enqueue_mutex);
    m_queue.push(v);
  }

  inline bool try_dequeue(T &value) override {
    std::lock_guard<std::mutex> lock(m_dequeue_mutex);
    if (m_queue.empty()) {
      return false;
    }
    value = m_queue.front();
    m_queue.pop();
    return true;
  }

  inline T dequeue() override {
    std::lock_guard<std::mutex> lock(m_dequeue_mutex);
    T                           value{};
    if (m_queue.empty()) {
      return value;
    }
    value = m_queue.front();
    m_queue.pop();
    return value;
  }

  inline void clear() override {
    std::lock_guard<std::mutex> lock(m_enqueue_mutex);
    std::lock_guard<std::mutex> lock2(m_dequeue_mutex);
    std::queue<T>               empty;
    std::swap(m_queue, empty);
  }

private:
  mutable std::mutex m_enqueue_mutex;
  mutable std::mutex m_dequeue_mutex;
};