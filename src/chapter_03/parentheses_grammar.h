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

#ifndef MURRAYC_COMPILER_EXPERIMENTS_PARENTHESES_GRAMMAR_H
#define MURRAYC_COMPILER_EXPERIMENTS_PARENTHESES_GRAMMAR_H

#include "grammar.h"

/// The "Parentheses Grammar" from page 120, in section 3.4.1.
class ParenthesesGrammar {
public:
  // Non-terminals:
  static constexpr Symbol SYMBOL_GOAL = {"Goal"};
  static constexpr Symbol SYMBOL_LIST = {"List"};
  static constexpr Symbol SYMBOL_PAIR = {"Pair"};

  // Terminals:
  static constexpr Symbol SYMBOL_OPEN_PAREN = {"(", true};
  static constexpr Symbol SYMBOL_CLOSE_PAREN = {")", true};

  static constexpr Symbol SYMBOL_EMPTY = {"e", true};
  static constexpr Symbol SYMBOL_EOF = {"eof", true};

  // Not including SYMBOL_EMPTY.
  static constexpr std::array<Symbol, 6> symbols = {{
    SYMBOL_GOAL, SYMBOL_LIST, SYMBOL_PAIR, SYMBOL_OPEN_PAREN, SYMBOL_CLOSE_PAREN, SYMBOL_EOF}};

  static const GrammarRules rules;

  // Just for the action table and goto table,
  // as a one-off convenience.
  using GrammarRulesByNumber = std::unordered_map<std::size_t, std::pair<Symbol, Symbols>>;
  static const GrammarRulesByNumber rules_by_number;

  static Symbol
  recognise_word(const WordsMap& words_map, const std::string& word) {
    // A rather dumb implementation just to get things working:

    const auto iter = words_map.find(word);
    if (iter != std::end(words_map)) {
      return iter->second;
    }

    return SYMBOL_EMPTY;
  }

  static WordsMap
  build_words_map() {
    WordsMap result;

    const std::vector<Symbol> simple = {{SYMBOL_OPEN_PAREN, SYMBOL_CLOSE_PAREN}};
    for (const auto& symbol : simple) {
      result[symbol.name] = symbol;
    }

    // TODO: Don't use a word string for this.
    // It stops us from having a name called "eof".
    result["eof"] = SYMBOL_EOF;

    return result;
  }
};

const GrammarRules ParenthesesGrammar::rules = {
  {SYMBOL_GOAL,
    {{SYMBOL_LIST}}},
  {SYMBOL_LIST, {
    {SYMBOL_LIST, SYMBOL_PAIR},
    {SYMBOL_PAIR}}},
  {SYMBOL_PAIR, {
    {SYMBOL_OPEN_PAREN, SYMBOL_PAIR, SYMBOL_CLOSE_PAREN},
    {SYMBOL_OPEN_PAREN, SYMBOL_CLOSE_PAREN}}},
  };

// Based on Figure 3.16, in section 3.4.1, on page 120,
// of "Engineering a Compiler".
const ParenthesesGrammar::GrammarRulesByNumber ParenthesesGrammar::rules_by_number = {
  {1, {SYMBOL_GOAL, {SYMBOL_LIST}}},
  {2, {SYMBOL_LIST, {SYMBOL_LIST, SYMBOL_PAIR}}},
  {3, {SYMBOL_LIST, {SYMBOL_PAIR}}},
  {4, {SYMBOL_PAIR, {SYMBOL_OPEN_PAREN, SYMBOL_PAIR, SYMBOL_CLOSE_PAREN}}},
  {5, {SYMBOL_PAIR, {SYMBOL_OPEN_PAREN, SYMBOL_CLOSE_PAREN}}}
};

#endif // MURRAYC_COMPILER_EXPERIMENTS_PARENTHESES_GRAMMAR_H
