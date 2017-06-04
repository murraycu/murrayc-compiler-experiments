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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
 */

#include "bottom_up_lr1_parse.h"
#include "print_action_and_goto_tables.h"
#include "expression_grammar_with_ast.h"

#include <cassert>
#include <iostream>

static void
test_expression_grammar_with_load_tracking() {
  using Grammar = ExpressionGrammarWithAst;

  {
    Grammar::StoreType store;

    const std::vector<std::string> input = {"a", "+", "b", "x", "2"};
    const Symbols expected = {Grammar::SYMBOL_NAME, Grammar::SYMBOL_PLUS,
      Grammar::SYMBOL_NAME, Grammar::SYMBOL_MULTIPLY, Grammar::SYMBOL_NUM};
    const auto result = bottom_up_lr1_parse<Grammar>(input, store);
    assert(result.symbols == expected);

    // This (bad) test assumes all names are real, and all numbers are integer.

    const auto node = result.value;
    assert(node);
    assert(node->value().type == Grammar::Type::REAL);

    // TODO: Print AST.
  }
}

int
main() {
  test_expression_grammar_with_load_tracking();

  return 0;
}
