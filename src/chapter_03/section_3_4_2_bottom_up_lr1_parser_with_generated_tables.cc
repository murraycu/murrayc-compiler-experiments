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

#include "build_sets.h"
#include "parentheses_grammar.h"
#include "symbol.h"

#include <algorithm>
#include <cassert>
#include <iostream>
#include <map>
#include <set>
#include <stack>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

/*
static
void print_symbol(const Symbol& symbol) {
  std::cout << symbol.name;
}

template <typename T_Container>
static
void print_symbols(const T_Container& symbols) {
  bool is_first = true;
  for (const auto& s : symbols) {
    if (!is_first) {
      std::cout << ", ";
    }

    is_first = false;

    print_symbol(s);
  }
}
*/

static const Symbol SYMBOL_ERROR = {"Error", true};

using State = int;

using Rule = std::pair<Symbol, Symbols>;

/** An LR(1) Item.
 * As described on page 125 of "Engineering a Compiler".
 */
class LR1Item {
public:
  // TODO: Why do we need to declare this. It isn't necessary for Symbol, for
  // instance.
  LR1Item(const Rule& production_in, std::size_t placeholder_in,
    const Symbol& lookahead_symbol_in)
  : production(production_in),
    placeholder(placeholder_in),
    lookahead_symbol(lookahead_symbol_in) {
  }

  bool
  operator==(const LR1Item& other) const {
    return production == other.production && placeholder == other.placeholder &&
           lookahead_symbol == other.lookahead_symbol;
  }

  bool
  operator<(const LR1Item& other) const {
    if (production != other.production) {
      return production < other.production;
    }

    if (placeholder != other.placeholder) {
      return placeholder < other.placeholder;
    }

    return lookahead_symbol < other.lookahead_symbol;
  }

  Rule production;

  /** The position in the production's right-hand side (expansion).
   * 0 is before the start.
   */
  std::size_t placeholder = 0;

  Symbol lookahead_symbol;
};

/**
 * A set of LR(1) Items.
 */
using CCSet = std::set<LR1Item>;

/** The Canonical Collection.
 * This is a set of sets of LR(1) items.
 * As described on page 127 of "Engineering a Compiler".
 */
using CC = std::set<CCSet>;

/**
 * @param pos If this is 0 zero the result will be {0, symbols}.
 */
static std::pair<Symbols, Symbols>
split_rule(const Symbols& symbols, std::size_t pos) {
  assert(symbols.size() >= pos);
  const Symbols a(std::begin(symbols), std::begin(symbols) + pos);
  const Symbols b(std::begin(symbols) + pos, std::end(symbols));
  return {a, b};
}

/*
static void
print_rule(const Rule& rule) {
  print_symbol(rule.first);
  std::cout << " -> ";
  print_symbols(rule.second);
}

static
void print_cc_set(const CCSet& ccset) {
  for (const auto& item : ccset) {
    std::cout << "{";
    print_rule(item.production);
    std::cout << "}, " << item.placeholder << ", ";
    print_symbol(item.lookahead_symbol);
    std::cout << std::endl;
  }
}
*/

/**
 * Based on the pseudo code in Figure 3.20, in section 3.4.2,
 * on page 128, in "Engineering a Compiler."
 */
template <typename T_Grammar>
static void
closure(CCSet& s, const FirstSets& first) {
  if (s.empty()) {
    return;
  }

  bool changing = true;
  while (changing) {
    for (const auto& ccset : s) {
      changing = false;

      const auto [b, cd] = split_rule(ccset.production.second, ccset.placeholder);
      if (cd.empty()) {
        continue;
      }

      const auto [cpart, d] = split_rule(cd, 1);
      assert(cpart.size() == 1);
      const auto c = cpart[0];
      const auto a = ccset.lookahead_symbol;

      const auto iter = T_Grammar::rules.find(c);
      if (iter == std::end(T_Grammar::rules)) {
        continue;
      }

      const auto& expansions = iter->second;
      for (const auto& y : expansions) {
        Symbols da = d;
        da.emplace_back(a);
        const auto firstda = build_first_set_for_symbols<T_Grammar>(first, da);

        for (const auto& b : firstda) {
          const auto old_count = s.size();
          const LR1Item item(Rule({c, y}), 0, b);
          s.emplace(item);
          changing |= (old_count != s.size());
        }
      }
    }
  }
}

/** Get the first symbol after the item's rules' placeholder position,
 * if any.
 * @result {true, symbol} if an symbol was found, else {false, ignored}.
 */
template <typename T_Grammar>
static std::pair<bool, Symbol>
get_symbol_after_placeholder(const LR1Item& item) {
  const auto& rule = item.production;
  const auto& expansion = rule.second;

  // Get the symbol following the placeholder, if any:
  const auto b_xd = split_rule(expansion, item.placeholder);
  const auto& xd = b_xd.second;
  if (xd.empty()) {
    return {false, T_Grammar::SYMBOL_EMPTY};
  }

  const auto xd_split = split_rule(xd, 1);
  const auto& x = xd_split.first;
  if (x.empty()) {
    return {false, T_Grammar::SYMBOL_EMPTY};
  }

  return {true, x[0]};
}

/**
 * Based on the pseudo code in Figure 3.21, in section 3.4.2,
 * on page 129, in "Engineering a Compiler."
 */
template <typename T_Grammar>
static CCSet
do_goto(const CCSet& s, const Symbol& x, const FirstSets& first) {
  CCSet moved;

  // Find any LR1Items in the CC set which have symbol x right
  // after the placeholder position, and create LR1Items which
  // move that placeholder position 1 symbol forward.
  // Then call closure() on the result.
  for (const auto& i : s) {
    const auto& rule = i.production;

    // Get the symbol following the placeholder, if any:
    const auto [found, x_maybe] = get_symbol_after_placeholder<T_Grammar>(i);
    if (!found) {
      continue;
    }

    // Check if that symbol is x:
    if (x_maybe != x) {
      continue;
    }

    // Create an LR(1) Item that is the same, but with the placeholder
    // position moved 1 step further:
    const auto& a = i.lookahead_symbol;
    const LR1Item item(rule, i.placeholder + 1, a);
    moved.emplace(item);
  }

  if (!moved.empty()) {
    closure<T_Grammar>(moved, first);
  }

  return moved;
}

template <typename T_Grammar>
static Rule
get_initial_rule() {
  const auto& rules = T_Grammar::rules;
  const auto iter = rules.find(T_Grammar::SYMBOL_GOAL);
  if (iter == std::end(rules)) {
    return {};
  }

  const auto& goal = iter->first;
  const auto& expansions = iter->second;

  // TODO: Support multiple expansions of Goal?
  assert(expansions.size() == 1);
  return {goal, expansions[0]};
}

template <typename T_Grammar>
static LR1Item
get_initial_lr1item() {
  const auto initial_rule = get_initial_rule<T_Grammar>();
  return {initial_rule, 0, T_Grammar::SYMBOL_EOF};
}

using CCSetIDs = std::map<CCSet, std::size_t>;

/** Build the Canonical Collection.
 *
 * Based on the pseudocode in Figure 3.22, in section 3.4.2,
 * on page 130, in "Engineering a Compiler".
 */
template <typename T_Grammar>
static void
build_cc(CC& cc, CCSetIDs& cc_ids, const FirstSets& first) {
  const auto initial_item = get_initial_lr1item<T_Grammar>();
  CCSet cc0 = {initial_item};
  closure<T_Grammar>(cc0, first);
  cc.emplace(cc0);

  cc_ids[cc0] = 0;

  std::set<CCSet> processed;

  State id = 1;
  bool changing = true;
  while (changing) {
    changing = false;

    for (const auto& cci : cc) {
      // Only examine sets that we have not yet processed:
      if (processed.count(cci)) {
        continue;
      }

      // Mark the set as processed:
      processed.emplace(cci);

      for (const auto& item : cci) {
        const auto [found, x] = get_symbol_after_placeholder<T_Grammar>(item);
        if (!found) {
          continue;
        }

        const auto temp = do_goto<T_Grammar>(cci, x, first);
        if (!cc.count(temp)) {
          changing = true;
          cc.emplace(temp);

          cc_ids[temp] = id;
          ++id;
        }

        // TODO: Record the transition from CCi to temp on symbol x.
      }
    }
  }
}

enum class ActionType {
  NONE,
  SHIFT,
  REDUCE,
  ACCEPT
};

class Action {
public:
  ActionType type = ActionType::NONE;
  std::size_t arg = 0;
};

using ActionTable = std::map<State, std::map<Symbol, Action>>;
using GotoTable = std::map<State, std::map<Symbol, std::size_t>>;

/** Build the action table and goto table needed for the table-driven
 * bottom-up LR(1) parser.
 *
 * Based on the pseudo code in Figure 3.24, in section 3.4.2, on page 134, in
 * "Engineering a compiler". But with a slightly different order of if/else
 * blocks.
 */
template <typename T_Grammar>
static void
build_action_and_goto_tables(ActionTable& action_table, GotoTable& goto_table) {
  // Get the rule numbers for each rule:
  std::map<Rule, std::size_t> rule_numbers;
  for (const auto& p : T_Grammar::rules_by_number) {
    const auto i = p.first;
    const auto& rule = p.second;
    rule_numbers[rule] = i;
  }

  const auto first = build_first_sets<T_Grammar>();
  CC cc;
  CCSetIDs cc_ids;
  build_cc<T_Grammar>(cc, cc_ids, first);

  for (const auto& cci : cc) {
    assert(cc_ids.count(cci));
    const auto i = cc_ids[cci];

    for (const auto& I : cci) {
      const auto& rule = I.production;
      const auto& a = I.lookahead_symbol;
      const auto placeholder_at_end = I.placeholder >= rule.second.size();

      const auto [found, c] = get_symbol_after_placeholder<T_Grammar>(I);
      if (found && c.terminal) {
        // I's rule is A -> B.cy
        //
        // Encountering c would be a valid next step towards discovering the
        // (nonterminal) left-hand side A. So we discover the next state, j,
        // via goto, and generate a shift action from this state, i, to that
        // state, j, via this symbol c.
        const auto ccj = do_goto<T_Grammar>(cci, c, first);
        assert(cc_ids.count(ccj));
        const auto j = cc_ids[ccj];
        action_table[i][c] = {ActionType::SHIFT, j};
      } else if (placeholder_at_end && rule.first == T_Grammar::SYMBOL_GOAL &&
                 a == T_Grammar::SYMBOL_EOF) {
        // I's rule is S' (Goal)-> S, the placeholder is at the end, and I's
        // lookahead is eof.
        //
        // The placeholder is at the end of the expansion, so this would mean
        // that the parser has recognized the whole expansion.  And because the
        // left-hand side the goal symbol, and the lookahead symbol is eof,
        // this is the accepting state.
        action_table[i][a] = {ActionType::ACCEPT, 0};
      } else if (placeholder_at_end) {
        // I's rule is A -> B, the placeholder is at the end, and I's lookahead
        // is a.
        //
        // The placeholder is at the end of the expansion, so this would mean
        // that the parser has recognized the whole expansion. This is then a
        // handle, on lookahead symbol a.
        assert(rule_numbers.count(rule));
        const auto rule_number = rule_numbers[rule];
        action_table[i][a] = {ActionType::REDUCE, rule_number};
      }

      // For each nonterminal:
      for (const auto& n : T_Grammar::symbols) {
        if (n.terminal) {
          continue;
        }

        const auto ccj = do_goto<T_Grammar>(cci, n, first);
        const auto iter = cc_ids.find(ccj);
        if (iter == std::end(cc_ids)) {
          // This is acceptable.
          // Not all parts of the goto table have entries.
          continue;
        }

        assert(cc_ids.count(ccj));
        const auto j = cc_ids[ccj];
        goto_table[i][n] = j;
      }
    }
  }
}

template <typename T_Grammar>
static bool
match(
  const WordsMap& words_map, const Symbol& symbol, const std::string& word) {
  const auto word_symbol = T_Grammar::recognise_word(words_map, word);
  return word_symbol == symbol;
}

// Just to hide the use of find() that is nececessary with
// a const std::map.
static Action
get_action_from_table(
  const ActionTable& table, State state, const Symbol& symbol) {
  const auto iteras = table.find(state);
  if (iteras == std::end(table)) {
    // Error.
    return {};
  }

  const auto& actions = iteras->second;

  const auto iteraa = actions.find(symbol);
  if (iteraa == std::end(actions)) {
    // Error.
    return {};
  }

  return iteraa->second;
}

// Just to hide the use of find() that is nececessary with
// a const std::map.
static std::size_t
get_goto_from_table(const GotoTable& table, State state, const Symbol& symbol) {
  const auto iteras = table.find(state);
  if (iteras == std::end(table)) {
    // Error.
    return {};
  }

  const auto& m = iteras->second;
  const auto iteraa = m.find(symbol);
  if (iteraa == std::end(m)) {
    // Error.
    return {};
  }

  return iteraa->second;
}
/** Based on the pseudocode in Figure 3.11, in section 3.3.3, on page 112,
 * of "Engineering a Compiler".
 */
template <typename T_Grammar>
static Symbols
bottom_up_lr1_parse(const std::vector<std::string>& words) {
  const auto n_words = words.size();
  if (n_words == 0) {
    return {};
  }

  ActionTable action_table;
  GotoTable goto_table;
  build_action_and_goto_tables<T_Grammar>(action_table, goto_table);

  // The pseudo code puts both the symbol and the state (number) on the same
  // stack,
  // pushing and popping two each time. That seems unnecessarily complicated.
  // Instead this uses 2 stacks, to simplify type safety.
  std::stack<Symbol> symbol_stack;
  std::stack<State> state_stack;

  symbol_stack.emplace(T_Grammar::SYMBOL_GOAL);
  state_stack.emplace(0);

  const auto words_map = T_Grammar::build_words_map();

  constexpr const char* WORD_EOF = "eof";

  std::size_t input_focus = 0;
  auto word = words[input_focus];

  Symbols result;
  while (true) {
    const auto state = state_stack.top();

    const auto symbol_for_word = T_Grammar::recognise_word(words_map, word);

    const auto action =
      get_action_from_table(action_table, state, symbol_for_word);

    if (action.type == ActionType::REDUCE) {
      // Get the A -> B rule:
      assert(T_Grammar::rules_by_number.count(action.arg));
      const auto& rule = T_Grammar::rules_by_number.at(action.arg);
      const auto& a = rule.first;
      const auto& b = rule.second;

      for (auto i = 0u; i < b.size(); ++i) {
        state_stack.pop();
        symbol_stack.pop();
      }

      symbol_stack.emplace(a);

      const auto prev_state = state_stack.top();
      const auto next_state = get_goto_from_table(goto_table, prev_state, a);
      state_stack.emplace(next_state);

    } else if (action.type == ActionType::SHIFT) {
      const auto next_state = action.arg;

      symbol_stack.push(symbol_for_word);
      state_stack.push(next_state);

      if (symbol_for_word.terminal) {
        result.emplace_back(symbol_for_word);
      }

      ++input_focus;
      word = input_focus >= n_words ? WORD_EOF : words[input_focus];
    } else if (action.type == ActionType::ACCEPT) {
      break;
    } else {
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

  const CCSet expected = {
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_PAIR, Grammar::SYMBOL_CLOSE_PAREN}}, 1, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_PAIR, Grammar::SYMBOL_CLOSE_PAREN}}, 1, Grammar::SYMBOL_OPEN_PAREN},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_CLOSE_PAREN}}, 1, Grammar::SYMBOL_EOF},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_CLOSE_PAREN}}, 1, Grammar::SYMBOL_OPEN_PAREN},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_PAIR, Grammar::SYMBOL_CLOSE_PAREN}}, 0, Grammar::SYMBOL_CLOSE_PAREN},
    {{Grammar::SYMBOL_PAIR, {Grammar::SYMBOL_OPEN_PAREN, Grammar::SYMBOL_CLOSE_PAREN}}, 0, Grammar::SYMBOL_CLOSE_PAREN}
  };

  const auto result = do_goto<Grammar>(cc0, Grammar::SYMBOL_OPEN_PAREN, first);
  assert(result == expected);

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
  std::unordered_set<State> ids;
  for (const auto& p : cc_ids) {
    const auto i = p.second;
    assert(ids.count(i) == 0);

    ids.emplace(i);
    // std::cout << i << std::endl;
  }
}

/*
static void
print_action(const Action& action) {
  std::string str;
  switch (action.type) {
    case ActionType::SHIFT:
      str = "s";
      break;
    case ActionType::REDUCE:
      str = "r";
      break;
    case ActionType::ACCEPT:
      str = "a";
      break;
    default:
      str = "?";
      break;
  }

  std::cout << str << action.arg;
}
*/

static void
test_action_and_goto() {
  using Grammar = ParenthesesGrammar;

  ActionTable action_table;
  GotoTable goto_table;
  build_action_and_goto_tables<Grammar>(action_table, goto_table);

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

  /*
  std::cout << "Action Table:" << std::endl;
  for (const auto p : action_table) {
    const auto state = p.first;
    std::cout << state << ":" << std::endl;

    const auto& actions = p.second;
    for (const auto& p2 : actions) {
      const auto& symbol = p2.first;
      const auto& action = p2.second;

      std::cout << "  ";
      print_symbol(symbol);
      std::cout << ": ";
      print_action(action);
      std::cout << std::endl;
    }
    std::cout << std::endl;
  }

  std::cout << std::endl;
  std::cout << "Goto Table:" << std::endl;
  for (const auto p : goto_table) {
    const auto state = p.first;
    std::cout << state << ":" << std::endl;

    const auto& actions = p.second;
    for (const auto& p2 : actions) {
      const auto& symbol = p2.first;
      const auto j = p2.second;

      std::cout << "  ";
      print_symbol(symbol);
      std::cout << ": " << j << std::endl;
    }

    std::cout << std::endl;
  }
  */
}

int
main() {
  test_first_set_for_symbols();
  test_closure();
  test_goto();
  test_cc();
  test_action_and_goto();

  {
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

  return 0;
}
