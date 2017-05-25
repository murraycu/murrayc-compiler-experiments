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

#include "right_recursive_expression_grammar.h"
#include "build_sets.h"
#include "symbol.h"

#include <algorithm>
#include <cassert>
#include <iostream>
#include <map>
#include <set>
#include <stack>
#include <string>
#include <vector>

template <typename T_Container>
static void
print_symbols(const T_Container& symbols) {
  bool is_first = true;
  for (const auto& s : symbols) {
    if (!is_first) {
      std::cout << ", ";
    }

    is_first = false;

    std::cout << s.name;
  }
}

/** See Figure 3.11, in section 3.3.3, on page 112, of "Engineering a
 * Compiler".
 *
 * A map of non-terminals and terminals to productions (expansions).  TODO: The
 * book uses a rule (production) number rather than storing the expansion
 * itself. That would be more efficient.
 */
using Table = std::map<Symbol, std::map<Symbol, Symbols>>;

static const Symbol SYMBOL_ERROR = {"Error", true};

/** Create an LL(1) parse table for a right-recursive expression grammar.
 *
 * Based on the pseudocode in Figure 3.12, in section 3.3.3,
 * on page 111, of "Engineering a Compiler".
 */
template <typename T_Grammar>
static Table
build_table() {
  Table table;

  const auto first = build_first_sets<T_Grammar>();
  const auto follow = build_follow_sets<T_Grammar>(first);
  const auto first_for_rules = build_first_sets_for_rules<T_Grammar>(first);
  const auto first_plus =
    build_first_plus_sets<T_Grammar>(first_for_rules, follow);

  const auto& rules = T_Grammar::rules;

  const auto& symbols = T_Grammar::symbols;
  for (const auto& a : symbols) {
    if (a.terminal) {
      continue;
    }

    auto& table_a = table[a];

    for (const auto& w : symbols) {
      if (w.terminal) {
        table_a[w] = {SYMBOL_ERROR};
      }
    }

    const auto iterr = rules.find(a);
    if (iterr == std::end(rules)) {
      continue;
    }

    const auto& expansions = iterr->second;
    for (const auto& b : expansions) {
      const auto p = std::make_pair(a, b);

      const auto iter = first_plus.find(p);
      if (iter == std::end(first_plus)) {
        continue;
      }

      const auto& first_plus_ab = iter->second;
      for (const auto& w : first_plus_ab) {
        if (w.terminal) {
          table_a[w] = b;
        }
      }

      if (contains(first_plus_ab, T_Grammar::SYMBOL_EOF)) {
        table_a[T_Grammar::SYMBOL_EOF] = b;
      }
    }
  }

  // Print the table, for debug information:
  /*
  for (const auto& pair : table) {
    const auto& nt = pair.first;
    const auto& table_nt = pair.second;
    std::cout << nt.name << ":" << std::endl;
    for (const auto& pair2 : table_nt) {
      const auto& t = pair2.first;
      const auto& p = pair2.second;

      std::cout << "  " << t.name << ": ";
      print_symbols(p);
      std::cout << std::endl;
    }
  }
  */

  return table;
}

template <typename T_Grammar>
static bool
match(
  const WordsMap& words_map, const Symbol& symbol, const std::string& word) {
  const auto word_symbol = T_Grammar::recognise_word(words_map, word);
  return word_symbol == symbol;
}

/** Based on the pseudocode in Figure 3.11, in section 3.3.3, on page 112,
 * of "Engineering a Compiler".
 */
template <typename T_Grammar>
static Symbols
top_down_parse(const std::vector<std::string>& words) {
  const auto n_words = words.size();
  if (n_words == 0) {
    return {};
  }

  // We gradually build up the answer in this stack:
  std::stack<Symbol> st;
  st.emplace(T_Grammar::SYMBOL_EOF);
  st.emplace(T_Grammar::SYMBOL_GOAL);

  const auto table = build_table<T_Grammar>();
  const auto words_map = T_Grammar::build_words_map();

  constexpr const char* WORD_EOF = "eof";

  std::size_t input_focus = 0;
  std::string word = words[input_focus];

  Symbols result;
  while (true) {
    const auto focus = st.top();
    if (focus == T_Grammar::SYMBOL_EOF && word == WORD_EOF) {
      // Success
      break;
    }

    if (focus.terminal || focus == T_Grammar::SYMBOL_EOF) {
      if (match<T_Grammar>(words_map, focus, word)) {
        st.pop();
        result.emplace_back(focus);

        // Next word:
        ++input_focus;
        word = input_focus >= n_words ? WORD_EOF : words[input_focus];
      } else {
        // Error looking for symbol at top of stack.
        return {SYMBOL_ERROR};
      }
    } else {
      const auto iter = table.find(focus);
      if (iter == std::end(table)) {
        // Error expanding focus.
        return {SYMBOL_ERROR};
      }

      const auto& tablea = iter->second;

      // focus is a non-terminal:
      const auto word_symbol = T_Grammar::recognise_word(words_map, word);
      const auto iterb = tablea.find(word_symbol);
      if (iterb == std::end(tablea)) {
        // Error expanding focus.
        return {SYMBOL_ERROR};
      }

      const auto& b = iterb->second;

      st.pop();

      const auto k = b.size();
      for (int i = (k - 1); i >= 0; --i) {
        const auto& bi = b[i];

        if (!(bi == T_Grammar::SYMBOL_EMPTY)) {
          st.emplace(bi);
        }
      }
    }
  }

  return result;
}

int
main() {
  {
    // The "right-recursive variant of the classic expression grammar" from page
    // 101, in section 3.3.1.
    using Grammar = RightRecursiveGrammar;

    {
      auto table = build_table<Grammar>();
      assert(!table.empty());

      // These expected results are based on the table in Figure 3.11 (b), in
      // section 3.3.3,
      // on page 112, of "Engineering a Compiler".

      // The table should not have any terminals.
      const Symbols expected_plus_plus = {}; // No rule
      assert(table[Grammar::SYMBOL_PLUS][Grammar::SYMBOL_PLUS] ==
             expected_plus_plus);

      const Symbols expected_goal_eof = {SYMBOL_ERROR}; // No rule.
      assert(
        table[Grammar::SYMBOL_GOAL][Grammar::SYMBOL_EOF] == expected_goal_eof);

      const Symbols expected_goal_open_paren = {
        Grammar::SYMBOL_EXPR}; // Rule 0.
      assert(table[Grammar::SYMBOL_GOAL][Grammar::SYMBOL_OPEN_PAREN] ==
             expected_goal_open_paren);

      const Symbols expected_expr_prime_eof = {
        Grammar::SYMBOL_EMPTY}; // Rule 4.
      assert(table[Grammar::SYMBOL_EXPR_PRIME][Grammar::SYMBOL_EOF] ==
             expected_expr_prime_eof);

      const Symbols expected_factor_open_paren = {Grammar::SYMBOL_OPEN_PAREN,
        Grammar::SYMBOL_EXPR, Grammar::SYMBOL_CLOSE_PAREN}; // Rule 9.
      assert(table[Grammar::SYMBOL_FACTOR][Grammar::SYMBOL_OPEN_PAREN] ==
             expected_factor_open_paren);
    }

    {
      // Valid input:
      const std::vector<std::string> input = {"a", "+", "b", "x", "c"};
      const Symbols expected = {Grammar::SYMBOL_NAME, Grammar::SYMBOL_PLUS,
        Grammar::SYMBOL_NAME, Grammar::SYMBOL_MULTIPLY, Grammar::SYMBOL_NAME};
      assert(top_down_parse<Grammar>(input) == expected);
    }

    {
      // Invalid input:
      const std::vector<std::string> input = {"x", "+", "*", "y"};
      const Symbols expected = {SYMBOL_ERROR};
      assert(top_down_parse<Grammar>(input) == expected);
    }
  }

  return 0;
}
