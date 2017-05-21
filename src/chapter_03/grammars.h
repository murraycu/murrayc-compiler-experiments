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
#include <vector>

// A set of symbols, such as a possible expansion, or a full parse.
using Symbols = std::vector<Symbol>;

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
  static const Symbol SYMBOL_GOAL;
  static const Symbol SYMBOL_EXPR;
  static const Symbol SYMBOL_EXPR_PRIME;
  static const Symbol SYMBOL_TERM;
  static const Symbol SYMBOL_TERM_PRIME;
  static const Symbol SYMBOL_FACTOR;

  // Terminals:
  static const Symbol SYMBOL_PLUS;
  static const Symbol SYMBOL_MINUS;
  static const Symbol SYMBOL_MULTIPLY;
  static const Symbol SYMBOL_DIVIDE;
  static const Symbol SYMBOL_OPEN_PAREN;
  static const Symbol SYMBOL_CLOSE_PAREN;
  static const Symbol SYMBOL_NUM;
  static const Symbol SYMBOL_NAME;

  static const Symbol SYMBOL_EMPTY;
  static const Symbol SYMBOL_EOF;

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


const Symbol ClassicGrammar::SYMBOL_GOAL = {"Goal"};
const Symbol ClassicGrammar::SYMBOL_EXPR = {"Expr"};
const Symbol ClassicGrammar::SYMBOL_EXPR_PRIME = {"Expr'"};
const Symbol ClassicGrammar::SYMBOL_TERM = {"Term"};
const Symbol ClassicGrammar::SYMBOL_TERM_PRIME = {"Term'"};
const Symbol ClassicGrammar::SYMBOL_FACTOR = {"Factor"};
const Symbol ClassicGrammar::SYMBOL_PLUS = {"+", true};
const Symbol ClassicGrammar::SYMBOL_MINUS = {"-", true};
const Symbol ClassicGrammar::SYMBOL_MULTIPLY = {"x", true};
const Symbol ClassicGrammar::SYMBOL_DIVIDE = {"÷", true};
const Symbol ClassicGrammar::SYMBOL_OPEN_PAREN = {"(", true};
const Symbol ClassicGrammar::SYMBOL_CLOSE_PAREN = {")", true};
const Symbol ClassicGrammar::SYMBOL_NUM = {"num", true};
const Symbol ClassicGrammar::SYMBOL_NAME = {"name", true};
const Symbol ClassicGrammar::SYMBOL_EMPTY = {"e", true};
const Symbol ClassicGrammar::SYMBOL_EOF = {"eof", true};

// Not including SYMBOL_EMPTY or SYMBOL_EOF.
const Symbols ClassicGrammar::symbols = {
  SYMBOL_GOAL, SYMBOL_EXPR, SYMBOL_EXPR_PRIME, SYMBOL_TERM, SYMBOL_TERM_PRIME, SYMBOL_FACTOR,
  SYMBOL_PLUS, SYMBOL_MINUS, SYMBOL_MULTIPLY, SYMBOL_DIVIDE, SYMBOL_OPEN_PAREN, SYMBOL_CLOSE_PAREN, SYMBOL_NUM, SYMBOL_NAME};

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
  static const Symbol SYMBOL_GOAL;
  static const Symbol SYMBOL_EXPR;
  static const Symbol SYMBOL_EXPR_PRIME;
  static const Symbol SYMBOL_TERM;
  static const Symbol SYMBOL_TERM_PRIME;
  static const Symbol SYMBOL_FACTOR;

  // Terminals:
  static const Symbol SYMBOL_PLUS;
  static const Symbol SYMBOL_MINUS;
  static const Symbol SYMBOL_MULTIPLY;
  static const Symbol SYMBOL_DIVIDE;
  static const Symbol SYMBOL_OPEN_PAREN;
  static const Symbol SYMBOL_CLOSE_PAREN;
  static const Symbol SYMBOL_NUM;
  static const Symbol SYMBOL_NAME;

  static const Symbol SYMBOL_EMPTY;
  static const Symbol SYMBOL_EOF;

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

const Symbol RightRecursiveGrammar::SYMBOL_GOAL = {"Goal"};
const Symbol RightRecursiveGrammar::SYMBOL_EXPR = {"Expr"};
const Symbol RightRecursiveGrammar::SYMBOL_EXPR_PRIME = {"Expr'"};
const Symbol RightRecursiveGrammar::SYMBOL_TERM = {"Term"};
const Symbol RightRecursiveGrammar::SYMBOL_TERM_PRIME = {"Term'"};
const Symbol RightRecursiveGrammar::SYMBOL_FACTOR = {"Factor"};
const Symbol RightRecursiveGrammar::SYMBOL_PLUS = {"+", true};
const Symbol RightRecursiveGrammar::SYMBOL_MINUS = {"-", true};
const Symbol RightRecursiveGrammar::SYMBOL_MULTIPLY = {"x", true};
const Symbol RightRecursiveGrammar::SYMBOL_DIVIDE = {"÷", true};
const Symbol RightRecursiveGrammar::SYMBOL_OPEN_PAREN = {"(", true};
const Symbol RightRecursiveGrammar::SYMBOL_CLOSE_PAREN = {")", true};
const Symbol RightRecursiveGrammar::SYMBOL_NUM = {"num", true};
const Symbol RightRecursiveGrammar::SYMBOL_NAME = {"name", true};
const Symbol RightRecursiveGrammar::SYMBOL_EMPTY = {"e", true};
const Symbol RightRecursiveGrammar::SYMBOL_EOF = {"eof", true};

// Not including SYMBOL_EMPTY or SYMBOL_EOF.
const Symbols RightRecursiveGrammar::symbols = {
  SYMBOL_GOAL, SYMBOL_EXPR, SYMBOL_EXPR_PRIME, SYMBOL_TERM, SYMBOL_TERM_PRIME, SYMBOL_FACTOR,
  SYMBOL_PLUS, SYMBOL_MINUS, SYMBOL_MULTIPLY, SYMBOL_DIVIDE, SYMBOL_OPEN_PAREN, SYMBOL_CLOSE_PAREN, SYMBOL_NUM, SYMBOL_NAME, SYMBOL_EMPTY};

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

#endif // MURRAYC_COMPILER_EXPERIMENTS_GRAMMARS_H
