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

#ifndef MURRAYC_COMPILER_EXPERIMENTS_SIGNED_BINARY_NUMBERS_GRAMMAR_H
#define MURRAYC_COMPILER_EXPERIMENTS_SIGNED_BINARY_NUMBERS_GRAMMAR_H

#include "grammar.h"

/// The "Signed Binary Numbers Grammar" from page 199, in section 4.4.
class SignedBinaryNumbersGrammar {
public:
  using ValueType = int;
  using StoreType = int; // Not used.

  // Non-terminals:
  static constexpr Symbol SYMBOL_GOAL = {"Goal"};
  static constexpr Symbol SYMBOL_NUMBER = {"Number"};
  static constexpr Symbol SYMBOL_SIGN = {"Sign"};
  static constexpr Symbol SYMBOL_LIST = {"List"};
  static constexpr Symbol SYMBOL_BIT = {"Bit"};

  // Terminals:
  static constexpr Symbol SYMBOL_0 = {"0", true};
  static constexpr Symbol SYMBOL_1 = {"1", true};
  static constexpr Symbol SYMBOL_PLUS = {"+", true};
  static constexpr Symbol SYMBOL_MINUS = {"-", true};

  static constexpr Symbol SYMBOL_EMPTY = {"e", true};
  static constexpr Symbol SYMBOL_EOF = {"eof", true};

  // Not including SYMBOL_EMPTY.
  static constexpr std::array<Symbol, 10> symbols = {{
    SYMBOL_GOAL, SYMBOL_NUMBER, SYMBOL_SIGN, SYMBOL_LIST, SYMBOL_BIT,
    SYMBOL_0, SYMBOL_1, SYMBOL_PLUS, SYMBOL_MINUS, SYMBOL_EOF}};


  static const GrammarRules<ValueType, StoreType> rules;

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

  static int
  on_rule_1(const int& /* store */, const std::vector<int>& values, const std::vector<WordType>& /* words */) {
    assert(!values.empty());

    // Just pass it up:
    return values[0];
  }

  static int
  on_rule_2(const int& /* store */, const std::vector<int>& values, const std::vector<WordType>& /* words */) {
    assert(values.size() == 2);

    const auto& sign_val = values[0]; 
    const auto& list_val = values[1]; 

    return sign_val * list_val;
  }

  static int
  on_rule_3(const int& /* store */, const std::vector<int>& /* values */, const std::vector<WordType>& /* words */) {
    return 1;
  }

  static int
  on_rule_4(const int& /* store */, const std::vector<int>& /* values */, const std::vector<WordType>& /* words */) {
    return -1;
  }

  static int
  on_rule_5(const int& /* store */, const std::vector<int>& values, const std::vector<WordType>& /* words */) {
    assert(values.size() == 1);

    const auto& bit_val = values[0];
    return bit_val;
  }

  static int
  on_rule_6(const int& /* store */, const std::vector<int>& values, const std::vector<WordType>& /* words */) {
    assert(values.size() == 2);

    const auto& list_val = values[0]; 
    const auto& bit_val = values[1]; 
    return 2 * list_val + bit_val;
  }

  static int
  on_rule_7(const int& /* store */, const std::vector<int>& /* values */, const std::vector<WordType>& /* words */) {
    return 0;
  }

  static int
  on_rule_8(const int& /* store */, const std::vector<int>& /* values */, const std::vector<WordType>& /* words */) {
    return 1;
  }
};

// Based on Figure 4.11, from section 4.4, on page 199,
// of "Engineering a Compiler".
// But without the subscripts, and with an extra rule for the goal symbol,
// for consistency with the other grammars.
const GrammarRules<SignedBinaryNumbersGrammar::ValueType, SignedBinaryNumbersGrammar::StoreType> SignedBinaryNumbersGrammar::rules = {
  {SYMBOL_GOAL, {
    {{SYMBOL_NUMBER}, &on_rule_1}}},
  {SYMBOL_NUMBER, {
    {{SYMBOL_SIGN, SYMBOL_LIST}, &on_rule_2}}},
  {SYMBOL_SIGN, {
    {{SYMBOL_PLUS}, &on_rule_3},
    {{SYMBOL_MINUS}, &on_rule_4}}},
  {SYMBOL_LIST, {
    {{SYMBOL_BIT}, &on_rule_5},
    {{SYMBOL_LIST, SYMBOL_BIT}, &on_rule_6}}},
  {SYMBOL_BIT, {
    {{SYMBOL_0}, &on_rule_7},
    {{SYMBOL_1}, &on_rule_8}}}
  };

#endif // MURRAYC_COMPILER_EXPERIMENTS_SIGNED_BINARY_NUMBERS_GRAMMAR_H
