/* Copyright (C) 2017 Murray Cumming
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

#ifndef MURRAYC_COMPILER_EXPERIMENTS_STATE_H
#define MURRAYC_COMPILER_EXPERIMENTS_STATE_H

#include <memory>
#include <set>
#include <string>
#include <unordered_map>
#include <cassert>

/** A State in an NFA or DFA,
 * with an ID, whether it is an accepting state, and a set of transitions to other states.
 *
 * The "Engineering a Compiler" book's text (pages 29 and 30) talks
 * instead about a set S of states, and separate δ(s, c) transition function,
 * but this is convenient for now.
 */
class State {
public:
  using char_t = char;
  static constexpr char_t E = '#'; // Because no simple C++ char type can represent a multi-character literal for ε.

  explicit State(const std::string& id, bool accepting = false)
  : id_(id), accepting_(accepting) {
    assert(!id.empty());
  }

  /** Add a state from this state, via the character @a ch,
   * to a next @a state with the @a id.
   */
  std::shared_ptr<State> add(char_t ch, const std::shared_ptr<State>& state) {
    states_[ch].emplace(state);
    return state;
  }

  /** Add a state from this state, via the character @a ch,
   * to a new next state with the @a id.
   */
  std::shared_ptr<State>
  add(char_t ch, const std::string& id, bool accepting = false) {
    auto s = std::make_shared<State>(id, accepting);
    return add(ch, s);
  }

  /*
  std::vector<char_t>
  transitions() {
    std::vector<char_t> result;

    std::transform(std::begin(states_), std::end(states_),
      std::inserter(result, std::end(result)),
        [](const auto& pair){
          return pair.first;
        });

    return result;
  }
  */

  using States = std::set<std::shared_ptr<State>>;

  decltype(auto)
  next_states(char_t ch) {
    auto iter = states_.find(ch);
    if (iter == std::end(states_)) {
      return States();
    }

    return iter->second;
  }

  decltype(auto)
  next_states_via_e() {
    return next_states(State::E);
  }

  std::string id() const {
    return id_;
  }

  bool is_accepting() const {
    return accepting_;
  }

private:
  std::string id_;
  bool accepting_ = false;
  std::unordered_map<char_t, States> states_;
};

#endif // MURRAYC_COMPILER_EXPERIMENTS_STATE_H
