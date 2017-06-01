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

#include "build_action_and_goto_tables.h"
#include "print_action_and_goto_tables.h"
#include "signed_binary_numbers_grammar.h"
#include "symbol.h"

#include <algorithm>
#include <cassert>
#include <iostream>
#include <map>
#include <set>
#include <stack>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

template <typename T_Grammar>
class ParseResult {
public:
  Symbols symbols;
  typename T_Grammar::ValueType value = typename T_Grammar::ValueType();
};

/** Based on the description in section 4.4.1,
 * and the end of section 4.4's introduction,
 * on page 199 of Engineering a Compiler.
 *
 * This is a modification of bottom_up_lr1_parse() in
 * section_3_4_2_bottom_up_lr1_parser_with_generated_tables.h
 *
 * This uses a grammar with code snippets associated with each grammar rule.
 * The code snippets are run after reach reduction on a rule,
 * getting a new left-hand value based on the right-hand values.
 */
template <typename T_Grammar>
static ParseResult<T_Grammar>
bottom_up_lr1_parse(const std::vector<std::string>& words) {
  const auto n_words = words.size();
  if (n_words == 0) {
    return {};
  }

  const auto rules = rules_by_number<T_Grammar>();

  ActionTable action_table;
  GotoTable goto_table;
  const auto built = build_action_and_goto_tables<T_Grammar>(action_table, goto_table);
  if (!built) {
    // A real parser would return some real clues.
    return {{SYMBOL_ERROR}, 0};
  }

  using StackElement = std::tuple<Symbol, State, typename T_Grammar::ValueType>;
  std::stack<StackElement> st;

  st.emplace(T_Grammar::SYMBOL_GOAL, 0, typename T_Grammar::ValueType());

  const auto words_map = T_Grammar::build_words_map();

  constexpr const char* WORD_EOF = "eof";

  std::size_t input_focus = 0;
  auto word = words[input_focus];

  Symbols result;
  while (true) {
    const auto state = std::get<1>(st.top());

    const auto symbol_for_word = T_Grammar::recognise_word(words_map, word);

    const auto action =
      get_action_from_table(action_table, state, symbol_for_word);

    if (action.type == Action::Type::REDUCE) {
      // Get the A -> B rule:
      assert(action.arg < rules.size());
      const auto& item = rules[action.arg];
      const auto& rule = item.production;
      const auto& a = rule.first;
      const auto& b = rule.second;

      // Pop the items from the stack, getting the values:
      std::vector<typename T_Grammar::ValueType> values;
      for (auto i = 0u; i < b.size(); ++i) {
        const auto& sitem = st.top();
        const auto& v = std::get<2>(sitem);
        values.emplace_back(v);

        st.pop();
      }

      const auto prev_state = std::get<1>(st.top());
      const auto next_state = get_goto_from_table(goto_table, prev_state, a);

      // Reverse the values so they are in the natural left-to-right order
      // for the callback:
      std::reverse(std::begin(values), std::end(values));

      const auto code = item.code;
      const auto value = code(values);
      st.emplace(a, next_state, value);
    } else if (action.type == Action::Type::SHIFT) {
      const auto next_state = action.arg;

      st.emplace(symbol_for_word, next_state, 0);

      if (symbol_for_word.terminal) {
        result.emplace_back(symbol_for_word);
      }

      ++input_focus;
      word = input_focus >= n_words ? WORD_EOF : words[input_focus];
    } else if (action.type == Action::Type::ACCEPT) {
      break;
    } else {
      return {{SYMBOL_ERROR}, 0};
    }
  }

  const auto& item = st.top();
  const auto& value = std::get<2>(item);
  return {result, value};
}

static void
test_signed_binary_numbers_grammar() {
  using Grammar = SignedBinaryNumbersGrammar;

  {
    // Valid input:
    const std::vector<std::string> input = {"+", "1"};
    const Symbols expected = {
      Grammar::SYMBOL_PLUS, Grammar::SYMBOL_1};
    const auto result = bottom_up_lr1_parse<Grammar>(input);
    assert(result.symbols == expected);
    assert(result.value == 1);
  }

  {
    // Valid input:
    const std::vector<std::string> input = {"-", "1"};
    const Symbols expected = {
      Grammar::SYMBOL_MINUS, Grammar::SYMBOL_1};
    const auto result = bottom_up_lr1_parse<Grammar>(input);
    assert(result.symbols == expected);
    assert(result.value == -1);
  }

  {
    // Valid input:
    const std::vector<std::string> input = {"+", "0"};
    const Symbols expected = {
      Grammar::SYMBOL_PLUS, Grammar::SYMBOL_0};
    const auto result = bottom_up_lr1_parse<Grammar>(input);
    assert(result.symbols == expected);
    assert(result.value == 0);
  }

  {
    // Valid input:
    const std::vector<std::string> input = {"-", "0"};
    const Symbols expected = {
      Grammar::SYMBOL_MINUS, Grammar::SYMBOL_0};
    const auto result = bottom_up_lr1_parse<Grammar>(input);
    assert(result.symbols == expected);
    assert(result.value == 0);
  }

  {
    // Valid input:
    const std::vector<std::string> input = {"+", "1", "1"};
    const Symbols expected = {
      Grammar::SYMBOL_PLUS, Grammar::SYMBOL_1, Grammar::SYMBOL_1};
    const auto result = bottom_up_lr1_parse<Grammar>(input);
    assert(result.symbols == expected);
    assert(result.value == 3);
  }

  {
    // Valid input:
    const std::vector<std::string> input = {"-", "1", "1"};
    const Symbols expected = {
      Grammar::SYMBOL_MINUS, Grammar::SYMBOL_1, Grammar::SYMBOL_1};
    const auto result = bottom_up_lr1_parse<Grammar>(input);
    assert(result.symbols == expected);
    assert(result.value == -3);
  }

  {
    // Valid input:
    const std::vector<std::string> input = {"+", "1", "0", "1"};
    const Symbols expected = {
      Grammar::SYMBOL_PLUS, Grammar::SYMBOL_1, Grammar::SYMBOL_0, Grammar::SYMBOL_1};
    const auto result = bottom_up_lr1_parse<Grammar>(input);
    assert(result.symbols == expected);
    assert(result.value == 5);
  }

  {
    // Valid input:
    const std::vector<std::string> input = {"-", "1", "0", "1"};
    const Symbols expected = {
      Grammar::SYMBOL_MINUS, Grammar::SYMBOL_1, Grammar::SYMBOL_0, Grammar::SYMBOL_1};
    const auto result = bottom_up_lr1_parse<Grammar>(input);
    assert(result.symbols == expected);
    assert(result.value == -5);
  }
}


int
main() {
  test_signed_binary_numbers_grammar();

  return 0;
}
