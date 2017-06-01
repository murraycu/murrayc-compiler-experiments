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

#ifndef MURRAYC_COMPILER_EXPERIMENTS_RIGHT_RECURSIVE_EXPRESSION_GRAMMAR_H
#define MURRAYC_COMPILER_EXPERIMENTS_RIGHT_RECURSIVE_EXPRESSION_GRAMMAR_H

#include "grammar.h"

/// The "right-recursive variant of the classic expression grammar" from page 101, in section 3.3.1.
class RightRecursiveExpressionGrammar {
public:
  // Non-terminals:
  static constexpr Symbol SYMBOL_GOAL = {"Goal"};
  static constexpr Symbol SYMBOL_EXPR = {"Expr"};
  static constexpr Symbol SYMBOL_EXPR_PRIME = {"Expr'"};
  static constexpr Symbol SYMBOL_TERM = {"Term"};
  static constexpr Symbol SYMBOL_TERM_PRIME = {"Term'"};
  static constexpr Symbol SYMBOL_FACTOR = {"Factor"};

  // Terminals:
  static constexpr Symbol SYMBOL_PLUS = {"+", true};
  static constexpr Symbol SYMBOL_MINUS = {"-", true};
  static constexpr Symbol SYMBOL_MULTIPLY = {"x", true};
  static constexpr Symbol SYMBOL_DIVIDE = {"÷", true};
  static constexpr Symbol SYMBOL_OPEN_PAREN = {"(", true};
  static constexpr Symbol SYMBOL_CLOSE_PAREN = {")", true};
  static constexpr Symbol SYMBOL_NUM = {"num", true};
  static constexpr Symbol SYMBOL_NAME = {"name", true};

  static constexpr Symbol SYMBOL_EMPTY = {"e", true};
  static constexpr Symbol SYMBOL_EOF = {"eof", true};

// Not including SYMBOL_EMPTY.
  static constexpr std::array<Symbol, 16> symbols = {{
    SYMBOL_GOAL, SYMBOL_EXPR, SYMBOL_EXPR_PRIME, SYMBOL_TERM, SYMBOL_TERM_PRIME, SYMBOL_FACTOR,
    SYMBOL_PLUS, SYMBOL_MINUS, SYMBOL_MULTIPLY, SYMBOL_DIVIDE, SYMBOL_OPEN_PAREN, SYMBOL_CLOSE_PAREN, SYMBOL_NUM, SYMBOL_NAME, SYMBOL_EOF}};

  /// The "right-recursive variant of the classic expression grammar" from page 101, in section 3.3.1.
  static const GrammarRules rules;

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

    // TODO: Don't use a word string for this.
    // It stops us from having a name called "eof".
    result["eof"] = SYMBOL_EOF;

    return result;
  }
};

const GrammarRules RightRecursiveExpressionGrammar::rules = {
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

#endif // MURRAYC_COMPILER_EXPERIMENTS_RIGHT_RECURSIVE_EXPRESSION_GRAMMAR_H
