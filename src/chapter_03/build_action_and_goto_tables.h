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

#ifndef MURRAYC_COMPILER_EXPERIMENTS_BUILD_ACTION_AND_GOTO_TABLES_H
#define MURRAYC_COMPILER_EXPERIMENTS_BUILD_ACTION_AND_GOTO_TABLES_H

#include "grammar.h"
#include "build_sets.h"

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
 * @param pos If this is 0 zero the result will be {{}, symbols}.
 */
static std::pair<Symbols, Symbols>
split_rule(const Symbols& symbols, std::size_t pos) {
  assert(symbols.size() >= pos);
  const Symbols a(std::begin(symbols), std::begin(symbols) + pos);
  const Symbols b(std::begin(symbols) + pos, std::end(symbols));
  return {a, b};
}

/*
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
          s.emplace(Rule({c, y}), 0, b);
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
  bool
  operator ==(const Action& other) const {
    return type == other.type &&
      arg == other.arg;
  }

  bool
  operator !=(const Action& other) const {
    return !operator==(other);
  }

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
 *
 * @param bool Whether the table building succeeded.
 */
template <typename T_Grammar>
static bool
build_action_and_goto_tables(ActionTable& action_table, GotoTable& goto_table) {
  const auto rules = rules_by_number<T_Grammar>();

  // Get the rule numbers for each rule:
  std::map<Rule, std::size_t> rule_numbers;
  const auto n_rules = rules.size();
  for (auto i = 0ul; i < n_rules; ++i) {
    const auto& rule = rules[i];
    rule_numbers[rule] = i;
  }

  const auto first = build_first_sets<T_Grammar>();
  CC cc;
  CCSetIDs cc_ids;
  build_cc<T_Grammar>(cc, cc_ids, first);

  for (const auto& cci : cc) {
    assert(cc_ids.count(cci));
    const auto i = cc_ids[cci];
    auto& action_table_i = action_table[i];

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
        const Action action = {ActionType::SHIFT, j};

        if (const auto iter = action_table_i.find(c); iter != std::end(action_table_i) &&
            action != iter->second) {
          // Conflict: We cannot have 2 actions in the same place.
          return false;
        }

        action_table_i[c] = action;
      } else if (placeholder_at_end && rule.first == T_Grammar::SYMBOL_GOAL &&
                 a == T_Grammar::SYMBOL_EOF) {
        // I's rule is S' (Goal)-> S, the placeholder is at the end, and I's
        // lookahead is eof.
        //
        // The placeholder is at the end of the expansion, so this would mean
        // that the parser has recognized the whole expansion.  And because the
        // left-hand side the goal symbol, and the lookahead symbol is eof,
        // this is the accepting state.
        const Action action = {ActionType::ACCEPT, 0};

        if (const auto iter = action_table_i.find(a); iter != std::end(action_table_i) &&
            action != iter->second) {
          // Conflict: We cannot have 2 actions in the same place.
          return false;
        }

        action_table_i[a] = action;
      } else if (placeholder_at_end) {
        // I's rule is A -> B, the placeholder is at the end, and I's lookahead
        // is a.
        //
        // The placeholder is at the end of the expansion, so this would mean
        // that the parser has recognized the whole expansion. This is then a
        // handle, on lookahead symbol a.
        assert(rule_numbers.count(rule));
        const auto rule_number = rule_numbers[rule];
        const Action action = {ActionType::REDUCE, rule_number};

        if (const auto iter = action_table_i.find(a); iter != std::end(action_table_i) &&
            action != iter->second) {
          // Conflict: We cannot have 2 actions in the same place.
          return false;
        }

        action_table_i[a] = action;
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

  return true;
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

#endif // MURRAYC_COMPILER_EXPERIMENTS_BUILD_ACTION_AND_GOTO_TABLES_H