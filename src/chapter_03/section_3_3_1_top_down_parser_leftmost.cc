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

#include "tree.h"

#include <algorithm>
#include <map>
#include <stack>
#include <string>
#include <vector>
#include <iostream>
#include <cassert>

// An enum would be more efficient.
class Symbol {
public:
  bool operator==(const Symbol& other) const {
    return name == other.name &&
      terminal == other.terminal;
  }

  bool operator<(const Symbol& other) const {
    if (name == other.name) {
      return terminal < other.terminal;
    }

    return name < other.name;
  }

  std::string name;
  bool terminal = false;
};

// Non-terminals:
static const Symbol SYMBOL_GOAL = {"Goal"};
static const Symbol SYMBOL_EXPR = {"Expr"};
static const Symbol SYMBOL_EXPR_PRIME = {"Expr'"};
static const Symbol SYMBOL_TERM = {"Term"};
static const Symbol SYMBOL_TERM_PRIME = {"Term'"};
static const Symbol SYMBOL_FACTOR = {"Factor"};

// Terminals:
static const Symbol SYMBOL_PLUS = {"+", true};
static const Symbol SYMBOL_MINUS = {"-", true};
static const Symbol SYMBOL_MULTIPLY = {"x", true};
static const Symbol SYMBOL_DIVIDE = {"÷", true};
static const Symbol SYMBOL_OPEN_PAREN = {"(", true};
static const Symbol SYMBOL_CLOSE_PAREN = {")", true};
static const Symbol SYMBOL_NUM = {"num", true};
static const Symbol SYMBOL_NAME = {"name", true};
static const Symbol SYMBOL_EMPTY = {"e", true};

// A set of symbols, such as a possible expansion, or a full parse.
using Symbols = std::vector<Symbol>;

// A set of possible expansions.
using Expansions = std::vector<Symbols>;

// A set of rules, mapping a symbol to its possible expansions.
// We could instead just have a flat set of rules (in a multimap),
// with more than one with the same left symbol.
// That might be what the pseudo code in Figure 3.2 is meant to use.
using GrammarRules = std::map<Symbol, std::vector<std::vector<Symbol>>>;

using WordsMap = std::map<std::string, Symbol>;

static Symbol
recognise_word(const WordsMap& words_map, const std::string& word) {
  // A rather dumb implementation just to get things working:

  const auto iter = words_map.find(word);
  if (iter != std::end(words_map)) {
    return iter->second;
  }

  return SYMBOL_NAME;
}

static WordsMap
build_words_map() {
  WordsMap result;

  const std::vector<Symbol> simple = {{SYMBOL_PLUS, SYMBOL_MINUS,
    SYMBOL_MULTIPLY, SYMBOL_DIVIDE, SYMBOL_OPEN_PAREN, SYMBOL_CLOSE_PAREN}};
  for (const auto& symbol : simple) {
    result[symbol.name] = symbol;
  }

  return result;
}

static bool
match(const WordsMap& words_map, const Symbol& symbol, const std::string& word) {
  const auto word_symbol  = recognise_word(words_map, word);
  return word_symbol == symbol;
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

static Symbols
top_down_parse(const GrammarRules& rules, const std::vector<std::string>& words) {
  // We gradually build up the answer in this stack:
  std::stack<Symbol> terminals;

  const auto n_words = words.size();
  if (n_words == 0) {
    return {};
  }

  constexpr const char* WORD_EOF = "eof";

  const auto words_map = build_words_map();

  std::size_t input_focus = 0;
  std::string word = words[input_focus];

  using Node = TreeNode<SymbolAndStatus>;

  // TODO: Don't leak.
  auto root = new Node({SYMBOL_GOAL, 0});
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
    } else if (focus_symbol == SYMBOL_EMPTY) {
      // The description on page 100 just says
      // "This E-production requires careful interpretation in the
      // parsing algorithm."
      // and there is no handling of it in the pseduocode.

      if (st.empty()) {
        break;
      }

      focus = st.top();
      st.pop();
    } else if (match(words_map, focus_symbol, word)) {
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
    const GrammarRules expression_grammar = {
      {SYMBOL_GOAL,
        {{SYMBOL_EXPR}}},
      {SYMBOL_EXPR, {
        {SYMBOL_EXPR, SYMBOL_PLUS, SYMBOL_TERM},
        {SYMBOL_EXPR, SYMBOL_MINUS, SYMBOL_TERM},
        {SYMBOL_TERM}}},
      {SYMBOL_TERM, {
        {SYMBOL_TERM, SYMBOL_MULTIPLY, SYMBOL_FACTOR},
        {SYMBOL_TERM, SYMBOL_DIVIDE, SYMBOL_FACTOR},
        {SYMBOL_FACTOR}}},
      {SYMBOL_FACTOR, {
        {SYMBOL_OPEN_PAREN, SYMBOL_EXPR, SYMBOL_CLOSE_PAREN},
        {SYMBOL_NUM},
        {SYMBOL_NAME}}}
      };

    {
      // Just to avoid a compiler warning about an unused declaration.
      // This code is not expected to work, and will in fact loop infinitely.
      const std::vector<std::string> input = {};
      const Symbols expected = {};
      assert(top_down_parse(expression_grammar, input) == expected);
    }

    {
      // This code is not expected to work, and will in fact loop infinitely,
      // due to infinite left recursion, caused by the grammar.
      // const std::vector<std::string> input = {"a", "+", "b", "x", "c"};
      // const Symbols expected = {SYMBOL_NAME, SYMBOL_PLUS, SYMBOL_NAME, SYMBOL_MULTIPLY, SYMBOL_NAME};
      // assert(top_down_parse(expression_grammar, input) == expected);
    }
  }

  {
    // The "right-recursive variant of the classic expression grammar" from page 101, in section 3.3.1.
    const GrammarRules expression_grammar = {
      {SYMBOL_GOAL,
        {{SYMBOL_EXPR}}},
      {SYMBOL_EXPR, {
        {SYMBOL_TERM, SYMBOL_EXPR_PRIME}}},
      {SYMBOL_EXPR_PRIME, {
        {SYMBOL_PLUS, SYMBOL_TERM, SYMBOL_EXPR_PRIME},
        {SYMBOL_MINUS, SYMBOL_TERM, SYMBOL_EXPR_PRIME},
        {SYMBOL_EMPTY}}},
      {SYMBOL_TERM, {
        {SYMBOL_FACTOR, SYMBOL_TERM_PRIME}}},
      {SYMBOL_TERM_PRIME, {
        {SYMBOL_MULTIPLY, SYMBOL_FACTOR, SYMBOL_TERM_PRIME},
        {SYMBOL_DIVIDE, SYMBOL_FACTOR, SYMBOL_TERM_PRIME},
        {SYMBOL_EMPTY}}},
      {SYMBOL_FACTOR, {
        {SYMBOL_OPEN_PAREN, SYMBOL_EXPR, SYMBOL_CLOSE_PAREN},
        {SYMBOL_NUM},
        {SYMBOL_NAME}}}
      };

    {
      // Just to avoid a compiler warning about an unused declaration.
      // This code is not expected to work, and will in fact loop infinitely.
      // Page 101 says "The example still assumes oracular choice."
      const std::vector<std::string> input = {};
      const Symbols expected = {};
      assert(top_down_parse(expression_grammar, input) == expected);
    }

    {
      const std::vector<std::string> input = {"a", "+", "b", "x", "c"};
      const Symbols expected = {SYMBOL_NAME, SYMBOL_PLUS, SYMBOL_NAME, SYMBOL_MULTIPLY, SYMBOL_NAME};
      assert(top_down_parse(expression_grammar, input) == expected);
    }
  }

  return 0;
}
