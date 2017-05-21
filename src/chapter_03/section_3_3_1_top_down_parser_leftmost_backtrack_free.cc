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

#include "symbol.h"
#include "tree.h"
#include "grammars.h"
#include "build_sets.h"

#include <algorithm>
#include <map>
#include <set>
#include <stack>
#include <string>
#include <vector>
#include <iostream>
#include <cassert>

template <typename T_Container>
static
void print_symbols(const T_Container& symbols) {
  bool is_first = true;
  for (const auto& s : symbols) {
    if (!is_first) {
      std::cout << ", ";
    }

    is_first = false;

    std::cout << s.name;
  }
}

template <typename T_Grammar>
static bool
match(const WordsMap& words_map, const Symbol& symbol, const std::string& word) {
  const auto word_symbol = T_Grammar::recognise_word(words_map, word);
  return word_symbol == symbol;
}

using SymbolSet = std::set<Symbol>;

template <typename T_Container, typename T_Value>
static T_Container
remove_symbol(const T_Container& symbols, const T_Value& symbol) {
  auto result = symbols;

  auto iter = result.find(symbol);
  if (iter != std::end(result)) {
    result.erase(iter);
  }

  /*
  result.erase(
    std::remove(std::begin(result), std::end(result), symbol),
    std::end(result));
  */
  return result;
}

template <typename T_Container, typename T_Value>
static bool
contains(const T_Container& symbols, const T_Value& symbol) {
  return std::find(std::begin(symbols), std::end(symbols), symbol) !=
    std::end(symbols);
}

template <typename T_Grammar>
static bool
check_grammar_is_backtrack_free() {
  const auto first = build_first_sets<T_Grammar>();
  const auto follow = build_follow_sets<T_Grammar>(first);
  const auto first_for_rules = build_first_sets_for_rules<T_Grammar>(first);
  const auto first_plus = build_first_plus_sets<T_Grammar>(first_for_rules, follow);

  const auto& rules = T_Grammar::rules;
  for (const auto& rule : rules) {
    const auto& a = rule.first;
    if (a.terminal) {
      continue;
    }

    const auto& expansions = rule.second;

    const auto k = expansions.size();
    if (k <= 1) {
      continue;
    }

    std::map<Symbol, std::size_t> counts;
    for (const auto& b : expansions) {
      const auto single_rule= std::make_pair(a, b);
      const auto iter = first_plus.find(single_rule);
      if (iter == std::end(first_plus)) {
        continue;
      }

      const auto first_plus_b = iter->second;
      for (const auto& s : first_plus_b) {
        counts[s]++;

        if (counts[s] > 1) {
          return false;
        }
      }
    }
  }

  return true;
}

/**
 * A left-hand symbol, and the index of its right-hand expansion (in its rule) that is currently being used.
 */
class SymbolAndStatus {
public:
  Symbol symbol;
  std::size_t expansions_used = 0; // 0 means no rules have been tried yet.

  // The index of this symbol in the expansion of which this symbol is a part.
  std::size_t symbol_index = 0;

  // The number of symbols in the expansion of which this symbol is a part.
  std::size_t symbols_count = 0;
};

template <typename T_Grammar>
static Symbols
top_down_parse(const std::vector<std::string>& words) {
  const auto& rules = T_Grammar::rules;

  // We gradually build up the answer in this stack:
  std::stack<Symbol> terminals;

  const auto n_words = words.size();
  if (n_words == 0) {
    return {};
  }

  const auto first = build_first_sets<T_Grammar>();

  constexpr const char* WORD_EOF = "eof";

  const auto words_map = T_Grammar::build_words_map();

  std::size_t input_focus = 0;
  std::string word = words[input_focus];

  using Node = TreeNode<SymbolAndStatus>;

  // TODO: Don't leak.
  auto root = new Node({T_Grammar::SYMBOL_GOAL, 0});
  auto focus = root;

  // The pseudocode seems to push a symbol onto the stack,
  // but pop a tree node off the stack,
  // so we use the node, which gives us a symbol.
  std::stack<Node*> st;

  while (true) {
    auto& focusv = focus->value();
    const auto& focus_symbol = focusv.symbol;
    if (!(focus_symbol.terminal)) {
      // Pick next rule to expand focus:
      const auto rule_iter = rules.find(focus_symbol);
      if (rule_iter == std::end(rules)) {
        return {}; // Error.
      }

      const auto& expansions = rule_iter->second;
      if (expansions.empty()) {
        return {}; // Error.
      }

      // This is the oracular part:
      // If this could pick the corect expansion each time,
      // instead of just picking the next untried one,
      // then it could avoid the need to backtrack.
      const auto n_expansions = expansions.size();
      auto& expansions_used = focusv.expansions_used;
      if (expansions_used >= n_expansions) {
        // TODO: Backtrack.
        return {};
      }

      // Pick the next unused expansion.
      // Remember which expansion we used,
      // so we can try the next one next time, if we backtrack.
      // The pseudocode in the book (Figure 3.2) doesn't
      // say how this should be done.
      expansions_used++;
      const auto& expansion = expansions[expansions_used - 1];
      if (expansion.empty()) {
        return {}; //Error
      }

      // Build nodes for b1, b2...bn as children of focus:

      // Push symbols bn to b2 on the stack:
      const auto n = expansion.size();
      for (auto i = n - 1; i > 0; --i) {
        const auto& symbol = expansion[i];
        auto child_node = focus->add({symbol, 0, i, n});
        st.emplace(child_node);
      }

      // Use b1:
      const auto& b1 = expansion[0];
      focus = focus->add({b1, 0, 0, n});
    } else if (focus_symbol == T_Grammar::SYMBOL_EMPTY) {
      // The description on page 100 just says
      // "This E-production requires careful interpretation in the
      // parsing algorithm."
      // and there is no handling of it in the pseduocode.

      if (st.empty()) {
        break;
      }

      focus = st.top();
      st.pop();
    } else if (match<T_Grammar>(words_map, focus_symbol, word)) {
      // Use it in the result:
      terminals.emplace(focus_symbol);

      ++input_focus;
      word = input_focus >= n_words ? WORD_EOF : words[input_focus];

      // For instance, use the next symbol in the used rule:
      if (st.empty()) {
        break;
      }
      focus = st.top();
      st.pop();
    } else if (word == WORD_EOF && focus == nullptr) {
      // The peudocode says "accept the input and return the root".
      break;
    } else {
      // backtrack
      // The pseudocode says
      // "sets focus to its parent in the partially-built parse tree and disconnects its children.
      // If an untried rule remains with focus on its left-hand side, the parse expands focus by
      // that rule."

      auto used = focusv.symbol_index;
      auto unused = focusv.symbols_count - used - 1;

      // Pop unused symbols (from this expression) from the stack.
      // The pseudo code, and its description, don't mention this, but it seems to be necessary.
      while (unused--) {
        st.pop();
      }

      // Decrement input_focus to move left back befor words we need to re-examine.
      input_focus -= used;

      // Remove mistakenly-added terminals from the result:
      while (used--) {
        terminals.pop();
      }

      focus = focus->pop_to_parent();
    }
  }

  Symbols result;
  result.reserve(terminals.size());
  while (!terminals.empty()) {
    result.emplace_back(terminals.top());
    terminals.pop();
  }

  std::reverse(std::begin(result), std::end(result));
  return result;
}

int main() {

  {
    // The "classic expression grammar" from page 93, in section 3.2.4.
    using Grammar = ClassicGrammar;

    {
      // Just to avoid a compiler warning about an unused declaration.
      // This code is not expected to work, and will in fact loop infinitely.
      const std::vector<std::string> input = {};
      const Symbols expected = {};
      assert(top_down_parse<Grammar>(input) == expected);
    }

    {
      // This code is not expected to work, and will in fact loop infinitely,
      // due to infinite left recursion, caused by the grammar.
      // const std::vector<std::string> input = {"a", "+", "b", "x", "c"};
      // const Symbols expected = {Grammar::SYMBOL_NAME, Grammar::SYMBOL_PLUS, Grammar::SYMBOL_NAME, Grammar::SYMBOL_MULTIPLY, Grammar::SYMBOL_NAME};
      // assert(top_down_parse<Grammar>(input) == expected);
    }

    {
      assert(check_grammar_is_backtrack_free<Grammar>() == false);
    }
  }

  {
    // The "right-recursive variant of the classic expression grammar" from page 101, in section 3.3.1.
    using Grammar = RightRecursiveGrammar;

    {
      // Just to avoid a compiler warning about an unused declaration.
      // This code is not expected to work, and will in fact loop infinitely.
      // Page 101 says "The example still assumes oracular choice."
      const std::vector<std::string> input = {};
      const Symbols expected = {};
      assert(top_down_parse<Grammar>(input) == expected);
    }

    // Test the FIRST sets:
    auto first = build_first_sets<Grammar>();
    {

      // From the tables on page 105:

      // Terminals:
      const SymbolSet expected_num = {Grammar::SYMBOL_NUM};
      const SymbolSet expected_name = {Grammar::SYMBOL_NAME};
      const SymbolSet expected_plus = {Grammar::SYMBOL_PLUS};
      const SymbolSet expected_open_paren = {Grammar::SYMBOL_OPEN_PAREN};
      assert(first[Grammar::SYMBOL_NUM] == expected_num);
      assert(first[Grammar::SYMBOL_NAME] == expected_name);
      assert(first[Grammar::SYMBOL_PLUS] == expected_plus);
      assert(first[Grammar::SYMBOL_OPEN_PAREN] == expected_open_paren);

      // Non-terminals:
      const SymbolSet expected_expr = {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_NAME, Grammar::SYMBOL_NUM};
      const SymbolSet expected_expr_prime = {Grammar::SYMBOL_PLUS, Grammar::SYMBOL_MINUS, Grammar::SYMBOL_EMPTY};
      const SymbolSet expected_term = {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_NAME, Grammar::SYMBOL_NUM};
      const SymbolSet expected_term_prime = {Grammar::SYMBOL_MULTIPLY, Grammar::SYMBOL_DIVIDE, Grammar::SYMBOL_EMPTY};
      const SymbolSet expected_factor = {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_NAME, Grammar::SYMBOL_NUM};

      assert(first[Grammar::SYMBOL_EXPR] == expected_expr);
      assert(first[Grammar::SYMBOL_EXPR_PRIME] == expected_expr_prime);
      assert(first[Grammar::SYMBOL_TERM] == expected_term);
      assert(first[Grammar::SYMBOL_TERM_PRIME] == expected_term_prime);
      assert(first[Grammar::SYMBOL_FACTOR] == expected_factor);
    }

    // Test the FOLLOW sets:
    auto follow = build_follow_sets<Grammar>(first);
    {
      // From the table on page 106:
      const SymbolSet expected_follow_expr = {Grammar::SYMBOL_EOF, Grammar::SYMBOL_CLOSE_PAREN};
      const SymbolSet expected_follow_expr_prime = {Grammar::SYMBOL_EOF, Grammar::SYMBOL_CLOSE_PAREN};
      const SymbolSet expected_follow_term = {Grammar::SYMBOL_EOF, Grammar::SYMBOL_PLUS, Grammar::SYMBOL_MINUS, Grammar::SYMBOL_CLOSE_PAREN};
      const SymbolSet expected_follow_term_prime = {Grammar::SYMBOL_EOF, Grammar::SYMBOL_PLUS, Grammar::SYMBOL_MINUS, Grammar::SYMBOL_CLOSE_PAREN};
      const SymbolSet expected_follow_factor = {Grammar::SYMBOL_EOF, Grammar::SYMBOL_PLUS, Grammar::SYMBOL_MINUS, Grammar::SYMBOL_MULTIPLY, Grammar::SYMBOL_DIVIDE, Grammar::SYMBOL_CLOSE_PAREN};

      assert(follow[Grammar::SYMBOL_EXPR] == expected_follow_expr);
      assert(follow[Grammar::SYMBOL_EXPR] == expected_follow_expr);
      assert(follow[Grammar::SYMBOL_EXPR_PRIME] == expected_follow_expr_prime);
      assert(follow[Grammar::SYMBOL_TERM] == expected_follow_term);
      assert(follow[Grammar::SYMBOL_TERM_PRIME] == expected_follow_term_prime);
      assert(follow[Grammar::SYMBOL_FACTOR] == expected_follow_factor);
    }

    // Test the FIRST sets for rules (not just for individual symbols):
    auto first_for_rules = build_first_sets_for_rules<Grammar>(first);
    {
      // From the table on page 107:
      const SymbolSet expected_expr_prime_to_empty = {Grammar::SYMBOL_EMPTY};
      const SymbolSet expected_term_prime_to_empty = {Grammar::SYMBOL_EMPTY};

      const auto key_expr_prime_to_empty = std::make_pair(Grammar::SYMBOL_EXPR_PRIME, Symbols({Grammar::SYMBOL_EMPTY}));
      assert(first_for_rules[key_expr_prime_to_empty] == expected_expr_prime_to_empty);

      const auto key_term_prime_to_empty = std::make_pair(Grammar::SYMBOL_TERM_PRIME, Symbols({Grammar::SYMBOL_EMPTY}));
      assert(first_for_rules[key_term_prime_to_empty] == expected_term_prime_to_empty);
    }

    // Test the FIRST+ sets:
    auto first_plus = build_first_plus_sets<Grammar>(first_for_rules, follow);
    {
      // From the table on page 107:
      const SymbolSet expected_expr_prime_to_empty = {Grammar::SYMBOL_EMPTY, Grammar::SYMBOL_EOF, Grammar::SYMBOL_CLOSE_PAREN};
      const SymbolSet expected_term_prime_to_empty = {Grammar::SYMBOL_EMPTY, Grammar::SYMBOL_EOF, Grammar::SYMBOL_PLUS,
        Grammar::SYMBOL_MINUS, Grammar::SYMBOL_CLOSE_PAREN};

      const auto key_expr_prime_to_empty = std::make_pair(Grammar::SYMBOL_EXPR_PRIME, Symbols({Grammar::SYMBOL_EMPTY}));
      assert(first_plus[key_expr_prime_to_empty] == expected_expr_prime_to_empty);

      const auto key_term_prime_to_empty = std::make_pair(Grammar::SYMBOL_TERM_PRIME, Symbols({Grammar::SYMBOL_EMPTY}));
      assert(first_plus[key_term_prime_to_empty] == expected_term_prime_to_empty);
    }

    {
      assert(check_grammar_is_backtrack_free<Grammar>() == true);
    }

    {
      const std::vector<std::string> input = {"a", "+", "b", "x", "c"};
      const Symbols expected = {Grammar::SYMBOL_NAME, Grammar::SYMBOL_PLUS, Grammar::SYMBOL_NAME, Grammar::SYMBOL_MULTIPLY, Grammar::SYMBOL_NAME};
      assert(top_down_parse<Grammar>(input) == expected);
    }
  }

  return 0;
}
