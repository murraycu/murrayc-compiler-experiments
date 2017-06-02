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

#ifndef MURRAYC_COMPILER_EXPERIMENTS_IF_THEN_ELSE_GRAMMAR_H
#define MURRAYC_COMPILER_EXPERIMENTS_IF_THEN_ELSE_GRAMMAR_H

#include "grammar.h"

/// The "IfThenElse Grammar" from page 120, in section 3.4.1.
class IfThenElseGrammar {
public:
  // Non-terminals:
  static constexpr Symbol SYMBOL_GOAL = {"Goal"};
  static constexpr Symbol SYMBOL_STMT = {"Stmt"};

  // Terminals:
  static constexpr Symbol SYMBOL_IF = {"if", true};
  static constexpr Symbol SYMBOL_EXPR = {"expr", true};
  static constexpr Symbol SYMBOL_THEN = {"then", true};
  static constexpr Symbol SYMBOL_ELSE = {"else", true};
  static constexpr Symbol SYMBOL_ASSIGN = {"assign", true};

  static constexpr Symbol SYMBOL_EMPTY = {"e", true};
  static constexpr Symbol SYMBOL_EOF = {"eof", true};

  // Not including SYMBOL_EMPTY.
  static constexpr std::array<Symbol, 8> symbols = {{
    SYMBOL_GOAL, SYMBOL_STMT, SYMBOL_IF, SYMBOL_EXPR, SYMBOL_THEN, SYMBOL_ELSE, SYMBOL_ASSIGN, SYMBOL_EOF}};

  static const GrammarRules rules;

  static Symbol
  recognise_word(const WordsMap& words_map, const WordType& word) {
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

    for (const auto& symbol : symbols) {
      if (!symbol.terminal) {
        continue;
      }

      result[symbol.name] = symbol;
    }

    // TODO: Don't use a word string for this.
    // It stops us from having a name called "eof".
    result["eof"] = SYMBOL_EOF;

    return result;
  }
};

const GrammarRules IfThenElseGrammar::rules = {
  {SYMBOL_GOAL,
    {{SYMBOL_STMT}}},
  {SYMBOL_STMT, {
    {SYMBOL_IF, SYMBOL_EXPR, SYMBOL_THEN, SYMBOL_STMT},
    {SYMBOL_IF, SYMBOL_EXPR, SYMBOL_THEN, SYMBOL_STMT, SYMBOL_ELSE, SYMBOL_STMT},
    {SYMBOL_ASSIGN}}}
  };


#endif // MURRAYC_COMPILER_EXPERIMENTS_IF_THEN_ELSE_GRAMMAR_H
