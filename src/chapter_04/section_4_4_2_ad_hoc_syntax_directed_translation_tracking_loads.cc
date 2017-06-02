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
#include "expression_grammar_with_load_tracking.h"

#include <cassert>
#include <iostream>


// This is from section 4.4.1.
static void
test_expression_grammar_with_load_tracking() {
  using Grammar = ExpressionGrammarWithLoadTracking;

  {
    Grammar::StoreType store;

    const std::vector<std::string> input = {"foo", "=", "a", "+", "b", "x", "c"};
    const Symbols expected = {Grammar::SYMBOL_NAME, Grammar::SYMBOL_EQUALS,
      Grammar::SYMBOL_NAME, Grammar::SYMBOL_PLUS, Grammar::SYMBOL_NAME,
      Grammar::SYMBOL_MULTIPLY, Grammar::SYMBOL_NAME};
    const auto result = bottom_up_lr1_parse<Grammar>(input, store);
    assert(result.symbols == expected);
    assert(store.cost == 12);
  }
}

int
main() {
  test_expression_grammar_with_load_tracking();

  return 0;
}
