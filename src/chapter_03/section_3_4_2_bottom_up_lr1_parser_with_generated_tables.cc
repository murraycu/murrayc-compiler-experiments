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

#include "build_action_and_goto_tables.h"
#include "print_action_and_goto_tables.h"
#include "parentheses_grammar.h"
#include "if_then_else_ambiguous_grammar.h"
#include "right_recursive_expression_grammar.h"
#include "signed_binary_numbers_grammar.h"
#include "classic_expression_grammar.h"
#include "symbol.h"

#include <algorithm>
#include <cassert>
#include <iostream>
#include <map>
#include <set>
#include <stack>
#include <string>
#include <unordered_set>
#include <vector>

/** Based on the pseudocode in Figure 3.15, in section 3.4.1, on page 119,
 * of "Engineering a Compiler".
 */
template <typename T_Grammar>
static Symbols
bottom_up_lr1_parse(const std::vector<std::string>& words) {
  const auto n_words = words.size();
  if (n_words == 0) {
    return {};
  }

  const auto rules = rules_by_number<T_Grammar>();

  ActionTable action_table;
  GotoTable goto_table;
  const auto built = build_action_and_goto_tables<T_Grammar>(action_table, goto_table);
  if (!built) {
    // A real parser would return some real clues.
    return {SYMBOL_ERROR};
  }

  // The pseudo code puts both the symbol and the state (number) on the same
  // stack,
  // pushing and popping two each time. That seems unnecessarily complicated.
  // Instead this uses a stack of pairs, to simplify type safety.
  using StackElement = std::pair<Symbol, State>;
  std::stack<StackElement> st;

  st.emplace(T_Grammar::SYMBOL_GOAL, 0);

  const auto words_map = T_Grammar::build_words_map();

  constexpr const char* WORD_EOF = "eof";

  std::size_t input_focus = 0;
  auto word = words[input_focus];

  Symbols result;
  while (true) {
    const auto state = st.top().second;
    const auto symbol_for_word = T_Grammar::recognise_word(words_map, word);

    // std::cout << "State: " << state << ", symbol: ";
    // print_symbol(symbol_for_word);
    // std::cout << ": " << std::endl;

    const auto action =
      get_action_from_table(action_table, state, symbol_for_word);

    if (action.type == Action::Type::REDUCE) {
      // Get the A -> B rule:
      assert(action.arg < rules.size());
      const auto& rule = rules[action.arg];

      // std::cout << "  Reduce: by rule: " << action.arg << " (";
      // print_rule(rule);
      // std::cout << "  )" << std::endl;

      const auto& a = rule.first;
      const auto& b = rule.second;

      // std::cout << "  Popping " << b.size() << " from stack." << std::endl;
      for (auto i = 0u; i < b.size(); ++i) {
        st.pop();
      }

      const auto prev_state = st.top().second;
      const auto next_state = get_goto_from_table(goto_table, prev_state, a);
      st.emplace(a, next_state);

      // std::cout << "  Moving from previous state " << prev_state << " to next state " << next_state << std::endl;
    } else if (action.type == Action::Type::SHIFT) {
      const auto next_state = action.arg;
      // std::cout << "  Shift: Moving to state " << next_state << std::endl;

      st.emplace(symbol_for_word, next_state);

      if (symbol_for_word.terminal) {
        result.emplace_back(symbol_for_word);
      }

      ++input_focus;
      word = input_focus >= n_words ? WORD_EOF : words[input_focus];
    } else if (action.type == Action::Type::ACCEPT) {
      // std::cout << "  Accept." << std::endl;
      break;
    } else {
      std::cerr << "State " << state << " has no action for symbol "
        << symbol_for_word.name << std::endl;
      std::cerr << "  parsed so far: ";
      for (const auto& symbol : result) {
        std::cerr << symbol.name << " ";
      }
      std::cerr << std::endl;

      return {SYMBOL_ERROR};
    }
  }

  return result;
}

static void
test_first_set_for_symbols() {
  using Grammar = ParenthesesGrammar;

  const Symbols test = {Grammar::SYMBOL_EOF};

  const auto first = build_first_sets<Grammar>();
  const auto result = build_first_set_for_symbols<Grammar>(first, test);
  assert(!result.empty());
}

static void
test_closure() {
  using Grammar = ParenthesesGrammar;

  const auto initial_item = get_initial_lr1item<Grammar>();
  CCSet cc0 = {initial_item};

  const auto first = build_first_sets<Grammar>();
  closure<Grammar>(cc0, first);

  // From page 129 of "Engineering a Compiler":
  const CCSet expected = {
    {initial_item},
    {{Grammar::SYMBOL_LIST, {Grammar::SYMBOL_LIST, Grammar::SYMBOL_PAIR}}, 0, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_LIST, {Grammar::SYMBOL_LIST, Grammar::SYMBOL_PAIR}}, 0, Grammar::SYMBOL_OPEN_PAREN},
    {{Grammar::SYMBOL_LIST, {Grammar::SYMBOL_PAIR}}, 0, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_LIST, {Grammar::SYMBOL_PAIR}}, 0, Grammar::SYMBOL_OPEN_PAREN},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_PAIR, Grammar::SYMBOL_CLOSE_PAREN}}, 0, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_PAIR, Grammar::SYMBOL_CLOSE_PAREN}}, 0, Grammar::SYMBOL_OPEN_PAREN},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_CLOSE_PAREN}}, 0, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_CLOSE_PAREN}}, 0, Grammar::SYMBOL_OPEN_PAREN}
  };
  // print_cc_set(cc0);
  assert(cc0 == expected);
}

static void
test_goto() {
  using Grammar = ParenthesesGrammar;

  const auto initial_rule = get_initial_rule<Grammar>();
  const Rule expected_initial_rule = {Grammar::SYMBOL_GOAL, {Grammar::SYMBOL_LIST}};

  CCSet cc0 = {
    {initial_rule, 0, Grammar::SYMBOL_EOF}
  };

  const auto first = build_first_sets<Grammar>();
  closure<Grammar>(cc0, first);

  // From page 131 of "Engineering a Compiler":
  const CCSet expected_cc1 = {
    {{Grammar::SYMBOL_GOAL, {Grammar::SYMBOL_LIST}}, 1, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_LIST, {Grammar::SYMBOL_LIST, Grammar::SYMBOL_PAIR}}, 1, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_LIST, {Grammar::SYMBOL_LIST, Grammar::SYMBOL_PAIR}}, 1, Grammar::SYMBOL_OPEN_PAREN},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_PAIR, Grammar::SYMBOL_CLOSE_PAREN}}, 0, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_PAIR, Grammar::SYMBOL_CLOSE_PAREN}}, 0, Grammar::SYMBOL_OPEN_PAREN},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_CLOSE_PAREN}}, 0, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_CLOSE_PAREN}}, 0, Grammar::SYMBOL_OPEN_PAREN}
  };
  const auto cc1 = do_goto<Grammar>(cc0, Grammar::SYMBOL_LIST, first);
  assert(cc1 == expected_cc1);

  // From page 131 of "Engineering a Compiler":
  const CCSet expected_cc2 = {
    {{Grammar::SYMBOL_LIST, {Grammar::SYMBOL_PAIR}}, 1, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_LIST, {Grammar::SYMBOL_PAIR}}, 1, Grammar::SYMBOL_OPEN_PAREN}
  };
  const auto cc2 = do_goto<Grammar>(cc0, Grammar::SYMBOL_PAIR, first);
  assert(cc2 == expected_cc2);

  // From page 132 of "Engineering a Compiler":
  const CCSet expected_cc3 = {
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_PAIR, Grammar::SYMBOL_CLOSE_PAREN}}, 1, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_PAIR, Grammar::SYMBOL_CLOSE_PAREN}}, 1, Grammar::SYMBOL_OPEN_PAREN},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_CLOSE_PAREN}}, 1, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_CLOSE_PAREN}}, 1, Grammar::SYMBOL_OPEN_PAREN},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_PAIR, Grammar::SYMBOL_CLOSE_PAREN}}, 0, Grammar::SYMBOL_CLOSE_PAREN},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_CLOSE_PAREN}}, 0, Grammar::SYMBOL_CLOSE_PAREN}
  };
  const auto cc3 = do_goto<Grammar>(cc0, Grammar::SYMBOL_OPEN_PAREN, first);
  assert(cc3 == expected_cc3);

  // From page 132 of "Engineering a Compiler":
  const CCSet expected_cc4 = {
    {{Grammar::SYMBOL_LIST, {Grammar::SYMBOL_LIST, Grammar::SYMBOL_PAIR}}, 2, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_LIST, {Grammar::SYMBOL_LIST, Grammar::SYMBOL_PAIR}}, 2, Grammar::SYMBOL_OPEN_PAREN}
  };
  const auto cc4 = do_goto<Grammar>(cc1, Grammar::SYMBOL_PAIR, first);
  assert(cc4 == expected_cc4);

  // From page 132 of "Engineering a Compiler":
  const CCSet expected_cc5 = {
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_PAIR, Grammar::SYMBOL_CLOSE_PAREN}}, 2, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_PAIR, Grammar::SYMBOL_CLOSE_PAREN}}, 2, Grammar::SYMBOL_OPEN_PAREN}
  };
  const auto cc5 = do_goto<Grammar>(cc3, Grammar::SYMBOL_PAIR, first);
  assert(cc5 == expected_cc5);

  // From page 132 of "Engineering a Compiler":
  const CCSet expected_cc6 = {
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_PAIR, Grammar::SYMBOL_CLOSE_PAREN}}, 0, Grammar::SYMBOL_CLOSE_PAREN},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_PAIR, Grammar::SYMBOL_CLOSE_PAREN}}, 1, Grammar::SYMBOL_CLOSE_PAREN},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_CLOSE_PAREN}}, 0, Grammar::SYMBOL_CLOSE_PAREN},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_CLOSE_PAREN}}, 1, Grammar::SYMBOL_CLOSE_PAREN}
  };
  const auto cc6 = do_goto<Grammar>(cc3, Grammar::SYMBOL_OPEN_PAREN, first);
  assert(cc6 == expected_cc6);

  // From page 132 of "Engineering a Compiler":
  const CCSet expected_cc7 = {
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_CLOSE_PAREN}}, 2, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_CLOSE_PAREN}}, 2, Grammar::SYMBOL_OPEN_PAREN}
  };
  const auto cc7 = do_goto<Grammar>(cc3, Grammar::SYMBOL_CLOSE_PAREN, first);
  assert(cc7 == expected_cc7);

  // From page 133 of "Engineering a Compiler":
  const CCSet expected_cc8 = {
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_PAIR, Grammar::SYMBOL_CLOSE_PAREN}}, 3, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_PAIR, Grammar::SYMBOL_CLOSE_PAREN}}, 3, Grammar::SYMBOL_OPEN_PAREN}
  };
  const auto cc8 = do_goto<Grammar>(cc5, Grammar::SYMBOL_CLOSE_PAREN, first);
  assert(cc8 == expected_cc8);

  // From page 133 of "Engineering a Compiler":
  const CCSet expected_cc9 = {
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_PAIR, Grammar::SYMBOL_CLOSE_PAREN}}, 2, Grammar::SYMBOL_CLOSE_PAREN},
  };
  const auto cc9 = do_goto<Grammar>(cc6, Grammar::SYMBOL_PAIR, first);
  assert(cc9 == expected_cc9);

  // From page 133 of "Engineering a Compiler":
  const CCSet expected_cc10 = {
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_CLOSE_PAREN}}, 2, Grammar::SYMBOL_CLOSE_PAREN},
  };
  const auto cc10 = do_goto<Grammar>(cc6, Grammar::SYMBOL_CLOSE_PAREN, first);
  assert(cc10 == expected_cc10);

  // From page 133 of "Engineering a Compiler":
  const CCSet expected_cc11 = {
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_PAIR, Grammar::SYMBOL_CLOSE_PAREN}}, 3, Grammar::SYMBOL_CLOSE_PAREN},
  };
  const auto cc11 = do_goto<Grammar>(cc9, Grammar::SYMBOL_CLOSE_PAREN, first);
  assert(cc11 == expected_cc11);
}

static void
test_cc() {
  using Grammar = ParenthesesGrammar;

  const auto first = build_first_sets<Grammar>();
  CC cc;
  CCSetIDs cc_ids;
  build_cc<Grammar>(cc, cc_ids, first);
  assert(cc.size() == 12);
  assert(cc_ids.size() == 12);

  // Check that there are no duplicates:
  std::unordered_map<State, CCSet> ids;
  for (const auto& p : cc_ids) {
    const auto i = p.second;
    assert(ids.count(i) == 0);

    ids[i] = p.first;
    // std::cout << i << std::endl;
  }

  /* We cannot do this because we don't generate the sets in
   * exactly the same order as in the walkthrough starting on page 131
   * of Engineering a Compiler. It would complicate the code unncessarily
   * to enforce that order.
  // From page 129 of "Engineering a Compiler":
  const CCSet expected_cc0 = {
    {{Grammar::SYMBOL_GOAL, {Grammar::SYMBOL_LIST}}, 0, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_LIST, {Grammar::SYMBOL_LIST, Grammar::SYMBOL_PAIR}}, 0, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_LIST, {Grammar::SYMBOL_LIST, Grammar::SYMBOL_PAIR}}, 0, Grammar::SYMBOL_OPEN_PAREN},
    {{Grammar::SYMBOL_LIST, {Grammar::SYMBOL_PAIR}}, 0, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_LIST, {Grammar::SYMBOL_PAIR}}, 0, Grammar::SYMBOL_OPEN_PAREN},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_PAIR, Grammar::SYMBOL_CLOSE_PAREN}}, 0, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_PAIR, Grammar::SYMBOL_CLOSE_PAREN}}, 0, Grammar::SYMBOL_OPEN_PAREN},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_CLOSE_PAREN}}, 0, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_CLOSE_PAREN}}, 0, Grammar::SYMBOL_OPEN_PAREN}
  };
  assert(ids[0] == expected_cc0);

  // From page 131 of "Engineering a Compiler":
  const CCSet expected_cc1 = {
    {{Grammar::SYMBOL_GOAL, {Grammar::SYMBOL_LIST}}, 1, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_LIST, {Grammar::SYMBOL_LIST, Grammar::SYMBOL_PAIR}}, 1, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_LIST, {Grammar::SYMBOL_LIST, Grammar::SYMBOL_PAIR}}, 1, Grammar::SYMBOL_OPEN_PAREN},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_PAIR, Grammar::SYMBOL_CLOSE_PAREN}}, 0, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_PAIR, Grammar::SYMBOL_CLOSE_PAREN}}, 0, Grammar::SYMBOL_OPEN_PAREN},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_CLOSE_PAREN}}, 0, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_CLOSE_PAREN}}, 0, Grammar::SYMBOL_OPEN_PAREN}
  };
  assert(ids[1] == expected_cc1);

  // From page 131 of "Engineering a Compiler":
  const CCSet expected_cc2 = {
    {{Grammar::SYMBOL_LIST, {Grammar::SYMBOL_PAIR}}, 1, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_LIST, {Grammar::SYMBOL_PAIR}}, 1, Grammar::SYMBOL_OPEN_PAREN}
  };
  assert(ids[2] == expected_cc2);

  // From page 132 of "Engineering a Compiler":
  const CCSet expected_cc3 = {
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_PAIR, Grammar::SYMBOL_CLOSE_PAREN}}, 0, Grammar::SYMBOL_CLOSE_PAREN},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_PAIR, Grammar::SYMBOL_CLOSE_PAREN}}, 1, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_PAIR, Grammar::SYMBOL_CLOSE_PAREN}}, 1, Grammar::SYMBOL_OPEN_PAREN},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_CLOSE_PAREN}}, 0, Grammar::SYMBOL_CLOSE_PAREN},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_CLOSE_PAREN}}, 1, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_CLOSE_PAREN}}, 1, Grammar::SYMBOL_OPEN_PAREN}
  };
  assert(ids[3] == expected_cc3);

  // From page 132 of "Engineering a Compiler":
  const CCSet expected_cc4 = {
    {{Grammar::SYMBOL_LIST, {Grammar::SYMBOL_LIST, Grammar::SYMBOL_PAIR}}, 2, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_LIST, {Grammar::SYMBOL_LIST, Grammar::SYMBOL_PAIR}}, 2, Grammar::SYMBOL_OPEN_PAREN}
  };
  assert(ids[4] == expected_cc4);

  // From page 132 of "Engineering a Compiler":
  const CCSet expected_cc5 = {
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_PAIR, Grammar::SYMBOL_CLOSE_PAREN}}, 2, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_PAIR, Grammar::SYMBOL_CLOSE_PAREN}}, 2, Grammar::SYMBOL_OPEN_PAREN}
  };
  print_cc_set(ids[5]);
  assert(ids[5] == expected_cc5);
  */
}

static void
test_action_and_goto() {
  using Grammar = ParenthesesGrammar;

  ActionTable action_table;
  GotoTable goto_table;
  const auto built = build_action_and_goto_tables<Grammar>(action_table, goto_table);
  assert(built);

  // See Figure 3.16 (b) on page 120 of "Engineering a Compiler".
  assert(action_table.size() == 12);
  assert(goto_table.size() == 4);

  // Check that there are no non-terminals in the action table:
  for (const auto& p : action_table) {
    for (const auto& p2 : p.second) {
      const auto& symbol = p2.first;
      assert(symbol.terminal);
    }
  }

  // Check that there are no terminals in the goto table:
  for (const auto& p : goto_table) {
    for (const auto& p2 : p.second) {
      const auto& symbol = p2.first;
      assert(!(symbol.terminal));
    }
  }

  // print_action_and_goto_tables<Grammar>(action_table, goto_table);

  /* We cannot do this because we don't generate the sets in
   * exactly the same order as in the walkthrough starting on page 131
   * of Engineering a Compiler. It would complicate the code unncessarily
   * to enforce that order.
  // Check that we have the same tables as in Figure 3.16, on page 120,
  // of Engineering a compiler:
  // (The rule numbers here are 0-indexed, but they are 1-indexed in the book.)
  const ActionTable expected_action_table = {
    {0, {
      {Grammar::SYMBOL_OPEN_PAREN, {Action::Type::SHIFT, 3}}
      }
    },
    {1, {
      {Grammar::SYMBOL_EOF, {Action::Type::ACCEPT, 0}},
      {Grammar::SYMBOL_OPEN_PAREN, {Action::Type::SHIFT, 3}}
      }
    },
    {2, {
      {Grammar::SYMBOL_EOF, {Action::Type::REDUCE, 2}},
      {Grammar::SYMBOL_OPEN_PAREN, {Action::Type::REDUCE, 2}}
      }
    },
    {3, {
      {Grammar::SYMBOL_OPEN_PAREN, {Action::Type::SHIFT, 6}},
      {Grammar::SYMBOL_CLOSE_PAREN, {Action::Type::SHIFT, 7}}
      }
    },
    {4, {
      {Grammar::SYMBOL_EOF, {Action::Type::REDUCE, 1}},
      {Grammar::SYMBOL_OPEN_PAREN, {Action::Type::REDUCE, 1}}
      }
    },
    {5, {
      {Grammar::SYMBOL_CLOSE_PAREN, {Action::Type::SHIFT, 8}}
      }
    },
    {6, {
      {Grammar::SYMBOL_OPEN_PAREN, {Action::Type::SHIFT, 6}},
      {Grammar::SYMBOL_CLOSE_PAREN, {Action::Type::SHIFT, 10}}
      }
    },
    {7, {
      {Grammar::SYMBOL_EOF, {Action::Type::REDUCE, 4}},
      {Grammar::SYMBOL_OPEN_PAREN, {Action::Type::REDUCE, 4}}
      }
    },
    {8, {
      {Grammar::SYMBOL_EOF, {Action::Type::REDUCE, 3}},
      {Grammar::SYMBOL_OPEN_PAREN, {Action::Type::REDUCE, 3}}
      }
    },
    {9, {
      {Grammar::SYMBOL_CLOSE_PAREN, {Action::Type::SHIFT, 11}}
      }
    },
    {10, {
      {Grammar::SYMBOL_CLOSE_PAREN, {Action::Type::REDUCE, 4}}
      }
    },
    {11, {
      {Grammar::SYMBOL_CLOSE_PAREN, {Action::Type::REDUCE, 3}}
      }
    }
  };
  assert(action_table == expected_action_table);
  */
}

static void
test_parentheses_grammar() {
  using Grammar = ParenthesesGrammar;

  {
    // Valid input:
    const std::vector<std::string> input = {"(", ")"};
    const Symbols expected = {
      Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_CLOSE_PAREN};
    assert(bottom_up_lr1_parse<Grammar>(input) == expected);
  }

  {
    // Valid input:
    const std::vector<std::string> input = {"(", "(", ")", ")", "(", ")"};
    const Symbols expected = {Grammar::SYMBOL_OPEN_PAREN,
      Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_CLOSE_PAREN,
      Grammar::SYMBOL_CLOSE_PAREN, Grammar::SYMBOL_OPEN_PAREN,
      Grammar::SYMBOL_CLOSE_PAREN};
    assert(bottom_up_lr1_parse<Grammar>(input) == expected);
  }

  {
    // Invalid input:
    const std::vector<std::string> input = {"(", ")", ")"};
    const Symbols expected = {SYMBOL_ERROR};
    assert(bottom_up_lr1_parse<Grammar>(input) == expected);
  }
}

static void
test_if_then_else_grammar() {
  using Grammar = IfThenElseGrammar;

  // We expect all these to fail, because the grammar is ambiguous.
  // See page 139 of "Engineering a compiler".

  ActionTable action_table;
  GotoTable goto_table;
  const auto built = build_action_and_goto_tables<Grammar>(action_table, goto_table);
  assert(!built);

  {
    // Valid input:
    const std::vector<std::string> input = {"if", "foo", "then", "bar"};
    const Symbols expected = {SYMBOL_ERROR};
    // const Symbols expected = {
    //  Grammar::SYMBOL_IF, Grammar::SYMBOL_EXPR, Grammar::SYMBOL_THEN, Grammar::SYMBOL_EXPR};
    assert(bottom_up_lr1_parse<Grammar>(input) == expected);
  }

  {
    // Valid input:
    const std::vector<std::string> input = {"if", "foo", "then", "bar", "else", "goo"};
    const Symbols expected = {SYMBOL_ERROR};
    // const Symbols expected = {
    //  Grammar::SYMBOL_IF, Grammar::SYMBOL_EXPR, Grammar::SYMBOL_THEN,
    //  Grammar::SYMBOL_EXPR, Grammar::SYMBOL_ELSE, Grammar::SYMBOL_EXPR};
    assert(bottom_up_lr1_parse<Grammar>(input) == expected);
  }

  {
    // Invalid input:
    const std::vector<std::string> input = {"if", "foo", "then", "else"};
    const Symbols expected = {SYMBOL_ERROR};
    assert(bottom_up_lr1_parse<Grammar>(input) == expected);
  }
}

static void
test_classic_expression_grammar() {
  using Grammar = ClassicExpressionGrammar;

  ActionTable action_table;
  GotoTable goto_table;
  const auto built = build_action_and_goto_tables<Grammar>(action_table, goto_table);
  assert(built);

  // This seems to work, even though there are conflicts while calling
  // build_action_and_goto_tables().
  {
    const std::vector<std::string> input = {"a", "+", "b", "x", "c"};
    const Symbols expected = {Grammar::SYMBOL_NAME, Grammar::SYMBOL_PLUS,
      Grammar::SYMBOL_NAME, Grammar::SYMBOL_MULTIPLY, Grammar::SYMBOL_NAME};
    assert(bottom_up_lr1_parse<Grammar>(input) == expected);
  }
}

static void
test_right_recursive_expression_grammar() {
  /*
  using Grammar = RightRecursiveExpressionGrammar;

  // TODO: Why doesn't this work?
  // Uncomment this to see error messages about the conflicts while building the tables.
  // Page 144 of "Engineering a Compiler" says "Bottom up parsers can accomodate either left or right recursion."
  ActionTable action_table;
  GotoTable goto_table;
  const auto built = build_action_and_goto_tables<Grammar>(action_table, goto_table);
  assert(!built);

  {
    // Valid input:
    const std::vector<std::string> input = {"a", "-", "2", "x", "c"};
    const Symbols expected = {
      Grammar::SYMBOL_NAME, Grammar::SYMBOL_MINUS, Grammar::SYMBOL_NUM, Grammar::SYMBOL_MULTIPLY, Grammar::SYMBOL_NAME};
    assert(bottom_up_lr1_parse<Grammar>(input) == expected);
  }
  */
}

// This grammar is actually from chapter 4,
// section 4.3, page 182, of
// "Engineering a Compiler",
// but this parser should be able to parse it.
static void
test_signed_binary_numbers_grammar() {
  using Grammar = SignedBinaryNumbersGrammar;

  {
    // Valid input:
    const std::vector<std::string> input = {"+", "1"};
    const Symbols expected = {
      Grammar::SYMBOL_PLUS, Grammar::SYMBOL_1};
    assert(bottom_up_lr1_parse<Grammar>(input) == expected);
  }

  {
    // Valid input:
    const std::vector<std::string> input = {"+", "0"};
    const Symbols expected = {
      Grammar::SYMBOL_PLUS, Grammar::SYMBOL_0};
    assert(bottom_up_lr1_parse<Grammar>(input) == expected);
  }

  {
    // Valid input:
    const std::vector<std::string> input = {"+", "1", "1"};
    const Symbols expected = {
      Grammar::SYMBOL_PLUS, Grammar::SYMBOL_1, Grammar::SYMBOL_1};
    assert(bottom_up_lr1_parse<Grammar>(input) == expected);
  }

  {
    // Valid input:
    const std::vector<std::string> input = {"+", "1", "0", "1"};
    const Symbols expected = {
      Grammar::SYMBOL_PLUS, Grammar::SYMBOL_1, Grammar::SYMBOL_0, Grammar::SYMBOL_1};
    assert(bottom_up_lr1_parse<Grammar>(input) == expected);
  }
}

int
main() {
  test_first_set_for_symbols();
  test_closure();
  test_goto();
  test_cc();
  test_action_and_goto();

  test_parentheses_grammar();
  test_if_then_else_grammar();
  test_classic_expression_grammar();
  test_right_recursive_expression_grammar();
  test_signed_binary_numbers_grammar();

  return 0;
}
