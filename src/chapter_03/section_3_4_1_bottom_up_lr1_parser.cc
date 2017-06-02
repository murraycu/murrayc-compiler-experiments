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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
 */

#include "parentheses_grammar.h"
#include "symbol.h"

#include <algorithm>
#include <cassert>
#include <iostream>
#include <map>
#include <set>
#include <stack>
#include <string>
#include <unordered_map>
#include <vector>

static const Symbol SYMBOL_ERROR = {"Error", true};

using Grammar = ParenthesesGrammar;

enum class ActionType {
  NONE,
  SHIFT,
  REDUCE,
  ACCEPT
};

using State = int;

class Action {
public:
  ActionType type = ActionType::NONE;
  std::size_t arg = 0;
};

using ActionTable = std::map<State, std::map<Symbol, Action>>;

// Based on Figure 3.16, in section 3.4.1, on page 120
// of "Enginnering a Compiler."
// But with 0-indexed rule numbers.
//
// Note: The rule numbers with the REDUCE actions depend on rules_by_number<>()
// filling its result with the rules in the expected orer, which seems fragile.
// That is not a problem for the generated tables.
const ActionTable action_table = {
  {
    0,
    {{Grammar::SYMBOL_EOF, {}},
      {Grammar::SYMBOL_OPEN_PAREN, {ActionType::SHIFT, 3}},
      {Grammar::SYMBOL_CLOSE_PAREN, {}}},
  },
  {
    1,
    {{Grammar::SYMBOL_EOF, {ActionType::ACCEPT, 0}},
      {Grammar::SYMBOL_OPEN_PAREN, {ActionType::SHIFT, 3}},
      {Grammar::SYMBOL_CLOSE_PAREN, {}}},
  },
  {
    2,
    {{Grammar::SYMBOL_EOF, {ActionType::REDUCE, 2}},
      {Grammar::SYMBOL_OPEN_PAREN, {ActionType::REDUCE, 2}},
      {Grammar::SYMBOL_CLOSE_PAREN, {}}},
  },
  {
    3,
    {{Grammar::SYMBOL_EOF, {}},
      {Grammar::SYMBOL_OPEN_PAREN, {ActionType::SHIFT, 6}},
      {Grammar::SYMBOL_CLOSE_PAREN, {ActionType::SHIFT, 7}}},
  },
  {
    4,
    {{Grammar::SYMBOL_EOF, {ActionType::REDUCE, 1}},
      {Grammar::SYMBOL_OPEN_PAREN, {ActionType::REDUCE, 1}},
      {Grammar::SYMBOL_CLOSE_PAREN, {}}},
  },
  {
    5,
    {{Grammar::SYMBOL_EOF, {}}, {Grammar::SYMBOL_OPEN_PAREN, {}},
      {Grammar::SYMBOL_CLOSE_PAREN, {ActionType::SHIFT, 8}}},
  },
  {
    6,
    {{Grammar::SYMBOL_EOF, {}},
      {Grammar::SYMBOL_OPEN_PAREN, {ActionType::SHIFT, 6}},
      {Grammar::SYMBOL_CLOSE_PAREN, {ActionType::SHIFT, 10}}},
  },
  {
    7,
    {{Grammar::SYMBOL_EOF, {ActionType::REDUCE, 4}},
      {Grammar::SYMBOL_OPEN_PAREN, {ActionType::REDUCE, 4}},
      {Grammar::SYMBOL_CLOSE_PAREN, {}}},
  },
  {
    8,
    {{Grammar::SYMBOL_EOF, {ActionType::REDUCE, 3}},
      {Grammar::SYMBOL_OPEN_PAREN, {ActionType::REDUCE, 3}},
      {Grammar::SYMBOL_CLOSE_PAREN, {}}},
  },
  {
    9,
    {{Grammar::SYMBOL_EOF, {}}, {Grammar::SYMBOL_OPEN_PAREN, {}},
      {Grammar::SYMBOL_CLOSE_PAREN, {ActionType::SHIFT, 11}}},
  },
  {
    10,
    {{Grammar::SYMBOL_EOF, {}}, {Grammar::SYMBOL_OPEN_PAREN, {}},
      {Grammar::SYMBOL_CLOSE_PAREN, {ActionType::REDUCE, 4}}},
  },
  {
    11,
    {{Grammar::SYMBOL_EOF, {}}, {Grammar::SYMBOL_OPEN_PAREN, {}},
      {Grammar::SYMBOL_CLOSE_PAREN, {ActionType::REDUCE, 3}}},
  }};

using GotoTable = std::map<State, std::map<Symbol, std::size_t>>;

// Based on Figure 3.16, in section 3.4.1, on page 120
// of "Enginnering a Compiler."
const GotoTable goto_table = {
  {
    0, {{Grammar::SYMBOL_LIST, 1}, {Grammar::SYMBOL_PAIR, 2}},
  },
  {
    1, {{Grammar::SYMBOL_LIST, 0}, {Grammar::SYMBOL_PAIR, 4}},
  },
  {
    2, {{Grammar::SYMBOL_LIST, 0}, {Grammar::SYMBOL_PAIR, 0}},
  },
  {
    3, {{Grammar::SYMBOL_LIST, 0}, {Grammar::SYMBOL_PAIR, 5}},
  },
  {
    4, {{Grammar::SYMBOL_LIST, 0}, {Grammar::SYMBOL_PAIR, 0}},
  },
  {
    5, {{Grammar::SYMBOL_LIST, 0}, {Grammar::SYMBOL_PAIR, 0}},
  },
  {
    6, {{Grammar::SYMBOL_LIST, 0}, {Grammar::SYMBOL_PAIR, 9}},
  },
  {
    7, {{Grammar::SYMBOL_LIST, 0}, {Grammar::SYMBOL_PAIR, 0}},
  },
  {
    8, {{Grammar::SYMBOL_LIST, 0}, {Grammar::SYMBOL_PAIR, 0}},
  },
  {
    9, {{Grammar::SYMBOL_LIST, 0}, {Grammar::SYMBOL_PAIR, 0}},
  },
  {
    10, {{Grammar::SYMBOL_LIST, 0}, {Grammar::SYMBOL_PAIR, 0}},
  },
  {
    11, {{Grammar::SYMBOL_LIST, 0}, {Grammar::SYMBOL_PAIR, 0}},
  }};

template <typename T_Grammar>
static bool
match(
  const Grammars::WordsMap& words_map, const Symbol& symbol, const Grammars::WordType& word) {
  const auto word_symbol = T_Grammar::recognise_word(words_map, word);
  return word_symbol == symbol;
}

// Just to hide the use of find() that is nececessary with
// a const std::map.
static Action
get_action_from_table(
  const ActionTable& table, State state, const Symbol& symbol) {
  const auto iteras = table.find(state);
  if (iteras == std::end(table)) {
    // Error.
    return {};
  }

  const auto& actions = iteras->second;

  const auto iteraa = actions.find(symbol);
  if (iteraa == std::end(actions)) {
    // Error.
    return {};
  }

  return iteraa->second;
}

// Just to hide the use of find() that is nececessary with
// a const std::map.
static std::size_t
get_goto_from_table(const GotoTable& table, State state, const Symbol& symbol) {
  const auto iteras = table.find(state);
  if (iteras == std::end(table)) {
    // Error.
    return {};
  }

  const auto& m = iteras->second;
  const auto iteraa = m.find(symbol);
  if (iteraa == std::end(m)) {
    // Error.
    return {};
  }

  return iteraa->second;
}
/** Based on the pseudocode in Figure 3.11, in section 3.3.3, on page 112,
 * of "Engineering a Compiler".
 */
template <typename T_Grammar>
static Symbols
bottom_up_lr1_parse(const std::vector<Grammars::WordType>& words) {
  const auto n_words = words.size();
  if (n_words == 0) {
    return {};
  }

  const auto rules = Grammars::rules_by_number<T_Grammar>();

  // The pseudo code puts both the symbol and the state (number) on the same
  // stack,
  // pushing and popping two each time. That seems unnecessarily complicated.
  // Instead this uses 2 stacks, to simplify type safety.
  std::stack<Symbol> symbol_stack;
  std::stack<State> state_stack;

  symbol_stack.emplace(T_Grammar::SYMBOL_GOAL);
  state_stack.emplace(0);

  const auto words_map = T_Grammar::build_words_map();

  constexpr const char* WORD_EOF = "eof";

  std::size_t input_focus = 0;
  auto word = words[input_focus];

  Symbols result;
  while (true) {
    const auto state = state_stack.top();

    const auto symbol_for_word = T_Grammar::recognise_word(words_map, word);

    const auto action =
      get_action_from_table(action_table, state, symbol_for_word);

    if (action.type == ActionType::REDUCE) {
      // Get the A -> B rule:
      assert(action.arg < rules.size());
      const auto& rule = rules[action.arg];
      const auto& a = rule.first;
      const auto& b = rule.second;

      for (auto i = 0u; i < b.size(); ++i) {
        state_stack.pop();
        symbol_stack.pop();
      }

      symbol_stack.emplace(a);

      const auto prev_state = state_stack.top();
      const auto next_state = get_goto_from_table(goto_table, prev_state, a);
      state_stack.emplace(next_state);

    } else if (action.type == ActionType::SHIFT) {
      const auto next_state = action.arg;

      symbol_stack.push(symbol_for_word);
      state_stack.push(next_state);

      if (symbol_for_word.terminal) {
        result.emplace_back(symbol_for_word);
      }

      ++input_focus;
      word = input_focus >= n_words ? WORD_EOF : words[input_focus];
    } else if (action.type == ActionType::ACCEPT) {
      break;
    } else {
      return {SYMBOL_ERROR};
    }
  }

  return result;
}

int
main() {
  {
    using Grammar = ParenthesesGrammar;

    {
      // Valid input:
      const std::vector<std::string> input = {"(", ")"};
      const Symbols expected = {
        Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_CLOSE_PAREN};
      assert(bottom_up_lr1_parse<Grammar>(input) == expected);
    }

    {
      // Valid input:
      const std::vector<std::string> input = {"(", "(", ")", ")", "(", ")"};
      const Symbols expected = {Grammar::SYMBOL_OPEN_PAREN,
        Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_CLOSE_PAREN,
        Grammar::SYMBOL_CLOSE_PAREN, Grammar::SYMBOL_OPEN_PAREN,
        Grammar::SYMBOL_CLOSE_PAREN};
      assert(bottom_up_lr1_parse<Grammar>(input) == expected);
    }

    {
      // Invalid input:
      const std::vector<std::string> input = {"(", ")", ")"};
      const Symbols expected = {SYMBOL_ERROR};
      assert(bottom_up_lr1_parse<Grammar>(input) == expected);
    }
  }

  return 0;
}
