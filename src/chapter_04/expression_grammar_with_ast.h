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

#ifndef MURRAYC_COMPILER_EXPERIMENTS_EXPRESSION_GRAMMAR_WITH_AST_H
#define MURRAYC_COMPILER_EXPERIMENTS_EXPRESSION_GRAMMAR_WITH_AST_H

#include "grammar.h"
#include "tree.h"
#include <unordered_set>

/** The "Building an Abstract Syntax Tree and inferring expression types"
 * grammar from page 205, in section 4.4.2.
 * See also Figure 4.7 and Figure 4.1.
 */
class ExpressionGrammarWithAst {
public:

  // See Figure 4.1 in "Engineering a Compiler".
  enum class Type {
    INTEGER,
    REAL,
    DOUBLE,
    COMPLEX,
    ILLEGAL
  };

  class Details {
  public:
    Symbol symbol;
    Grammars::WordType text;
    Type type;
  };

  using Node = TreeNode<Details>;
  using ValueType = Node*;

  class Store {
  public:
    // The root of the AST,
    // just so we can delete the whole tree later.
    Node* root = nullptr;
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

  static Node*
  on_rule_simple(StoreType& /* store */, const std::vector<Node*>& values, const std::vector<Grammars::WordType>& /* words */) {
    assert(!values.empty());
    return values[0];
  }

  static Node*
  on_rule_expr(StoreType& store, const std::vector<Node*>& values, const std::vector<Grammars::WordType>& /* words */) {
    assert(!values.empty());

    // Keep this top-level node so the Store's destructor can delete the whole tree.
    store.root = values[0];
    return store.root;
  }

  static Node*
  on_rule_plus(StoreType& /* store */, const std::vector<Node*>& values, const std::vector<Grammars::WordType>& /* words */) {
    assert(values.size() >= 3);
    auto a = values[0];
    assert(a);
    auto b = values[2];
    assert(b);
    const auto type = addition_result(a->value().type, b->value().type);

    return make_node(SYMBOL_PLUS, type, a, b);
  }

  static Node*
  on_rule_minus(StoreType& /* store */, const std::vector<Node*>& values, const std::vector<Grammars::WordType>& /* words */) {
    assert(values.size() >= 3);
    auto a = values[0];
    assert(a);
    auto b = values[2];
    assert(b);
    const auto type = subtraction_result(a->value().type, b->value().type);

    return make_node(SYMBOL_MINUS, type, a, b);
  }

  static Node*
  on_rule_multiply(StoreType& /* store */, const std::vector<Node*>& values, const std::vector<Grammars::WordType>& /* words */) {
    assert(values.size() >= 3);
    auto a = values[0];
    assert(a);
    auto b = values[2];
    assert(b);
    const auto type = multiplication_result(a->value().type, b->value().type);

    return make_node(SYMBOL_MULTIPLY, type, a, b);
  }

  static Node*
  on_rule_divide(StoreType& /* store */, const std::vector<Node*>& values, const std::vector<Grammars::WordType>& /* words */) {
    assert(values.size() >= 3);
    auto a = values[0];
    assert(a);
    auto b = values[2];
    assert(b);
    const auto type = division_result(a->value().type, b->value().type);

    return make_node(SYMBOL_DIVIDE, type, a, b);
  }

  static Node*
  on_rule_expr_in_parentheses(StoreType& /* store */, const std::vector<Node*>& values, const std::vector<Grammars::WordType>& /* words */) {
    assert(values.size() >= 3);
    return values[1];
  }

  static Node*
  on_rule_num(StoreType& /* store */, const std::vector<Node*>& /* values */, const std::vector<Grammars::WordType>& words) {
    // We assume that we already have a way to know the number's type.
    assert(!words.empty());
    return make_node(SYMBOL_NUM, words[0], Type::INTEGER);
  }

  static Node*
  on_rule_name(StoreType& /* store */, const std::vector<Node*>& /* values */, const std::vector<Grammars::WordType>& words) {
    // We assume that we already have a way to know the name's type.
    assert(!words.empty());
    return make_node(SYMBOL_NUM, words[0], Type::REAL);
  }

private:
  // See Figure 4.2
  constexpr static Type
  addition_result(Type a, Type b) {
    if (a == Type::INTEGER) {
      return b;
    } else if (b == Type::INTEGER) {
      return a;
    }

    if (a == Type::REAL) {
      return b;
    }

    if (b == Type::REAL) {
      return a;
    }

    if (a == Type::DOUBLE) {
      return b;
    }

    if (b == Type::DOUBLE) {
      return a;
    }

    return a; // Must be COMPLEX.
  }

  // Just reuse the addition logic, for convenience.
  constexpr static Type
  subtraction_result(Type a, Type b) {
    return addition_result(a, b);
  }

  // Just reuse the addition logic, for convenience.
  constexpr static Type
  multiplication_result(Type a, Type b) {
    return addition_result(a, b);
  }

  // Just reuse the addition logic, for convenience.
  constexpr static Type
  division_result(Type a, Type b) {
    return addition_result(a, b);
  }

  static Node*
  make_node(const Symbol& parent, const Grammars::WordType& word, Type type) {
    return new Node({parent, word, type});
  }

  static Node*
  make_node(const Symbol& parent, Type type, Node* a, Node* b) {
    const auto result = make_node(parent, Grammars::WordType(), type);
    result->add(a);
    result->add(b);
    return result;
  }

};

// Based on Figure 4.14, from section 4.4.2, on page 205,
// of "Engineering a Compiler".
// With an extra rule for the goal symbol, for consistency with the other grammars.
const Grammars::Rules<ExpressionGrammarWithAst::ValueType, ExpressionGrammarWithAst::StoreType> ExpressionGrammarWithAst::rules = {
  {SYMBOL_GOAL, {
    {{SYMBOL_EXPR}, &on_rule_expr}}},
  {SYMBOL_EXPR, {
    {{SYMBOL_EXPR, SYMBOL_PLUS, SYMBOL_TERM}, &on_rule_plus},
    {{SYMBOL_EXPR, SYMBOL_MINUS, SYMBOL_TERM}, &on_rule_minus},
    {{SYMBOL_TERM}, &on_rule_simple}}},
  {SYMBOL_TERM, {
    {{SYMBOL_TERM, SYMBOL_MULTIPLY, SYMBOL_FACTOR}, &on_rule_multiply},
    {{SYMBOL_TERM, SYMBOL_DIVIDE, SYMBOL_FACTOR}, &on_rule_divide},
    {{SYMBOL_FACTOR}, &on_rule_simple}}},
  {SYMBOL_FACTOR, {
    {{SYMBOL_OPEN_PAREN, SYMBOL_EXPR, SYMBOL_CLOSE_PAREN}, &on_rule_expr_in_parentheses},
    {{SYMBOL_NUM}, &on_rule_num},
    {{SYMBOL_NAME}, &on_rule_name}}}
  };

#endif // MURRAYC_COMPILER_EXPERIMENTS_EXPRESSION_GRAMMAR_WITH_AST_H
