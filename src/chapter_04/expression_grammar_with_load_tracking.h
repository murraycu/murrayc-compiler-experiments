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

#ifndef MURRAYC_COMPILER_EXPERIMENTS_EXPRESSION_GRAMMAR_WITH_LOAD_TRACKING_H
#define MURRAYC_COMPILER_EXPERIMENTS_EXPRESSION_GRAMMAR_WITH_LOAD_TRACKING_H

#include "grammar.h"
#include <unordered_set>

/// The "Tracking Loads" expression grammar from page 203, in section 4.4.2.
class ExpressionGrammarWithLoadTracking {
public:
  using ValueType = int;

  class Store {
  public:
    /// These are the costs of these operations.
    enum class Cost {
      STORE = 1,
      ADD = 1,
      SUBTRACT = 1,
      MULTIPLY = 1,
      DIVIDE = 1,
      LOAD = 3
    };

    std::unordered_set<Grammars::WordType> loaded;
    int cost = 0;
  };

  using StoreType = Store;

  // Non-terminals:
  static constexpr Symbol SYMBOL_GOAL = {"Goal"};
  static constexpr Symbol SYMBOL_BLOCK = {"Block"};
  static constexpr Symbol SYMBOL_ASSIGN = {"Assign"};
  static constexpr Symbol SYMBOL_EXPR = {"Expr"};
  static constexpr Symbol SYMBOL_TERM = {"Term"};
  static constexpr Symbol SYMBOL_FACTOR = {"Factor"};

  // Terminals:
  static constexpr Symbol SYMBOL_EQUALS = {"=", true};
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
    SYMBOL_GOAL, SYMBOL_BLOCK, SYMBOL_ASSIGN, SYMBOL_EXPR, SYMBOL_TERM, SYMBOL_FACTOR,
    SYMBOL_EQUALS, SYMBOL_PLUS, SYMBOL_MINUS, SYMBOL_MULTIPLY, SYMBOL_DIVIDE, SYMBOL_OPEN_PAREN, SYMBOL_CLOSE_PAREN, SYMBOL_NUM, SYMBOL_NAME, SYMBOL_EOF}};

  static const Grammars::Rules<ValueType, StoreType> rules;

  static Symbol
  recognise_word(const Grammars::WordsMap& words_map, const Grammars::WordType& word) {
    // A rather dumb implementation just to get things working:

    const auto iter = words_map.find(word);
    if (iter != std::end(words_map)) {
      return iter->second;
    }

    if (word.empty()) {
      return SYMBOL_EMPTY;
    }

    // Check the first character:
    const auto ch = word[0];
    if (ch >= '0' && ch <= '9') {
      return SYMBOL_NUM;
    }

    return SYMBOL_NAME;
  }

  static Grammars::WordsMap
  build_words_map() {
    Grammars::WordsMap result;

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
  on_rule_empty(StoreType& /* store */, const std::vector<int>& /* values */, const std::vector<Grammars::WordType>& /* words */) {
    return 0;
  }

  static int
  on_rule_assign(StoreType& store, const std::vector<int>& /* values */, const std::vector<Grammars::WordType>& /* words */) {
    store.cost += static_cast<int>(StoreType::Cost::STORE);

    return 0;
  }

  static int
  on_rule_plus(StoreType& store, const std::vector<int>& /* values */, const std::vector<Grammars::WordType>& /* words */) {
    store.cost += static_cast<int>(StoreType::Cost::ADD);

    return 0;
  }

  static int
  on_rule_minus(StoreType& store, const std::vector<int>& /* values */, const std::vector<Grammars::WordType>& /* words */) {
    store.cost += static_cast<int>(StoreType::Cost::SUBTRACT);

    return 0;
  }

  static int
  on_rule_multiply(StoreType& store, const std::vector<int>& /* values */, const std::vector<Grammars::WordType>& /* words */) {
    store.cost += static_cast<int>(StoreType::Cost::MULTIPLY);

    return 0;
  }

  static int
  on_rule_divide(StoreType& store, const std::vector<int>& /* values */, const std::vector<Grammars::WordType>& /* words */) {
    store.cost += static_cast<int>(StoreType::Cost::DIVIDE);

    return 0;
  }

  static int
  on_rule_load(StoreType& store, const std::vector<int>& /* values */, const std::vector<Grammars::WordType>& /* words */) {
    store.cost += static_cast<int>(StoreType::Cost::LOAD);

    return 0;
  }

  static int
  on_rule_name(StoreType& store, const std::vector<int>& /* values */, const std::vector<Grammars::WordType>& words) {
    assert(!words.empty());
    const Grammars::WordType name = words[0];
    assert(!name.empty());
    std::cout << "name: " << name << std::endl;

    if (!store.loaded.count(name)) {
      store.cost += static_cast<int>(StoreType::Cost::LOAD);
      store.loaded.emplace(name);
    }

    return 0;
  }
};

// Based on Figure 4.12 (based on Figure 4.8), from section 4.4.2, on page 203,
// of "Engineering a Compiler".
// With an extra rule for the goal symbol, for consistency with the other grammars.
const Grammars::Rules<ExpressionGrammarWithLoadTracking::ValueType, ExpressionGrammarWithLoadTracking::StoreType> ExpressionGrammarWithLoadTracking::rules = {
  {SYMBOL_GOAL, {
    {{SYMBOL_BLOCK}, &on_rule_empty}}},
  {SYMBOL_BLOCK, {
    {{SYMBOL_BLOCK, SYMBOL_ASSIGN}, &on_rule_empty},
    {{SYMBOL_ASSIGN}, &on_rule_empty}}},
  {SYMBOL_ASSIGN, {
    {{SYMBOL_NAME, SYMBOL_EQUALS, SYMBOL_EXPR}, &on_rule_assign}}},
  {SYMBOL_EXPR, {
    {{SYMBOL_EXPR, SYMBOL_PLUS, SYMBOL_TERM}, &on_rule_plus},
    {{SYMBOL_EXPR, SYMBOL_MINUS, SYMBOL_TERM}, &on_rule_minus},
    {{SYMBOL_TERM}, &on_rule_empty}}},
  {SYMBOL_TERM, {
    {{SYMBOL_TERM, SYMBOL_MULTIPLY, SYMBOL_FACTOR}, &on_rule_multiply},
    {{SYMBOL_TERM, SYMBOL_DIVIDE, SYMBOL_FACTOR}, &on_rule_divide},
    {{SYMBOL_FACTOR}, &on_rule_empty}}},
  {SYMBOL_FACTOR, {
    {{SYMBOL_OPEN_PAREN, SYMBOL_EXPR, SYMBOL_CLOSE_PAREN}, &on_rule_empty},
    {{SYMBOL_NUM}, &on_rule_load},
    {{SYMBOL_NAME}, &on_rule_name}}}
  };

#endif // MURRAYC_COMPILER_EXPERIMENTS_EXPRESSION_GRAMMAR_WITH_LOAD_TRACKING_H
