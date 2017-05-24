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

#ifndef MURRAYC_COMPILER_EXPERIMENTS_GRAMMARS_H
#define MURRAYC_COMPILER_EXPERIMENTS_GRAMMARS_H

#include "symbol.h"
#include <map>
#include <unordered_map>
#include <vector>

// A set of possible expansions.
using Expansions = std::vector<Symbols>;

// A set of rules (also known as productions, or productin rules), mapping a
// symbol to its possible expansions.  We could instead just have a flat set of
// rules (in a multimap), with more than one with the same left symbol.  That
// might be what the pseudo code in Figure 3.2 is meant to use.
using GrammarRules = std::map<Symbol, Expansions>;

using WordsMap = std::map<std::string, Symbol>;

// The "concept" for grammar classes:
// Grammar {
//  static const Symbols symbols;
//  static const GrammarRules rules;
//
//  static const Symbol SYMBOL_GOAL;
//  static const Symbol SYMBOL_EOF;
//  static const Symbol SYMBOL_EMPTY;
//
//  static Symbol
//  recognise_word(const WordsMap& words_map, const std::string& word);
//
//  static WordsMap
//  build_words_map();
// };

/// The "classic expression grammar" from page 93, in section 3.2.4.
class ClassicGrammar {
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

  static const Symbols symbols;

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

// Not including SYMBOL_EMPTY.
const Symbols ClassicGrammar::symbols = {
  SYMBOL_GOAL, SYMBOL_EXPR, SYMBOL_EXPR_PRIME, SYMBOL_TERM, SYMBOL_TERM_PRIME, SYMBOL_FACTOR,
  SYMBOL_PLUS, SYMBOL_MINUS, SYMBOL_MULTIPLY, SYMBOL_DIVIDE, SYMBOL_OPEN_PAREN, SYMBOL_CLOSE_PAREN, SYMBOL_NUM, SYMBOL_NAME, SYMBOL_EOF};

const GrammarRules ClassicGrammar::rules = {
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


/// The "right-recursive variant of the classic expression grammar" from page 101, in section 3.3.1.
class RightRecursiveGrammar {
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

  static const Symbols symbols;

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

// Not including SYMBOL_EMPTY.
const Symbols RightRecursiveGrammar::symbols = {
  SYMBOL_GOAL, SYMBOL_EXPR, SYMBOL_EXPR_PRIME, SYMBOL_TERM, SYMBOL_TERM_PRIME, SYMBOL_FACTOR,
  SYMBOL_PLUS, SYMBOL_MINUS, SYMBOL_MULTIPLY, SYMBOL_DIVIDE, SYMBOL_OPEN_PAREN, SYMBOL_CLOSE_PAREN, SYMBOL_NUM, SYMBOL_NAME, SYMBOL_EMPTY, SYMBOL_EOF};

const GrammarRules RightRecursiveGrammar::rules = {
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

  static const Symbols symbols;

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

// Not including SYMBOL_EMPTY or SYMBOL_EOF.
const Symbols ParenthesesGrammar::symbols = {
  SYMBOL_GOAL, SYMBOL_LIST, SYMBOL_PAIR, SYMBOL_OPEN_PAREN, SYMBOL_CLOSE_PAREN};

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

#endif // MURRAYC_COMPILER_EXPERIMENTS_GRAMMARS_H
