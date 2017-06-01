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

static
void print_lr1_item(const LR1Item& item) {
  const auto& rule = item.production;

  std::cout << "{";
  print_symbol(rule.first);
  std::cout << " -> ";

  bool is_first = true;
  std::size_t i = 0;
  for (const auto& s : rule.second) {
    if (i == item.placeholder) {
      std::cout << " . ";
    }

    if (!is_first) {
      std::cout << ", ";
    }

    is_first = false;

    print_symbol(s);
    ++i;
  }

  if (item.placeholder == rule.second.size()) {
    std::cout << " .";
  }

  std::cout << "}, ";
  print_symbol(item.lookahead_symbol);
  std::cout << std::endl;
}

static
void print_cc_set(const CCSet& ccset) {
  if (ccset.empty()) {
    std::cout << "(EMPTY STATE SET)" << std::endl;
  }

  for (const auto& item : ccset) {
    print_lr1_item(item);
  }
}

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

/**
 * Complete a state, by adding any LR(1) items that the input
 * LR(1) items, in @a s, imply.
 *
 * It does this by finding rules whose left hand side is
 * implied by the symbols, and the lookahead symbol, after
 * the placeholder (via the FIRST sets).
 *
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
    changing = false;

    // For each item [A -> B.Cd , a]:
    // (d could be empty)
    const auto scopy = s;
    for (const auto& item : scopy) {
      // Get the C symbol, after the placeholder:
      const auto [b, cd] = split_rule(item.production.second, item.placeholder);
      if (cd.empty()) {
        continue;
      }

      const auto [cpart, d] = split_rule(cd, 1);
      assert(cpart.size() == 1);
      const auto c = cpart[0];
      const auto a = item.lookahead_symbol;

      // Get any productions with C on the left-hand side (C -> y):
      const auto iter = T_Grammar::rules.find(c);
      if (iter == std::end(T_Grammar::rules)) {
        continue;
      }

      const auto& expansions = iter->second;
      for (const auto& items : expansions) {
        const auto& y = items.expansion;

        // For each b in FIRST(da):
        Symbols da = d;
        da.emplace_back(a);
        const auto firstda = build_first_set_for_symbols<T_Grammar>(first, da);

        for (const auto& b : firstda) {
          // Build an item [C -> .y , b]:
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

/** Discover the state (the set of LR(1) items) that the parser
 * would transition to from state @a s after recognizing a symbol @a x.
 *
 * It does this by finding the LR(1) items in which the placeholder
 * precedes the symbol @a x, and advancing the placeholder past that
 * symbol @a x.
 *
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

  // Complete the set to match a state in the canonical closure.
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

  const auto& item = expansions[0];
  return {goal, item.expansion};
}

template <typename T_Grammar>
static LR1Item
get_initial_lr1item() {
  const auto initial_rule = get_initial_rule<T_Grammar>();
  return {initial_rule, 0, T_Grammar::SYMBOL_EOF};
}

using CCSetIDs = std::map<CCSet, std::size_t>;

void
print_cc(const CCSetIDs& cc_ids) {
  // Get them in numerical order of state:
  std::unordered_map<State, CCSet> ids;
  for (const auto& p : cc_ids) {
    const auto i = p.second;
    assert(ids.count(i) == 0);

    ids[i] = p.first;
  }

  std::cout << "Canonical Collection:" << std::endl;
  const auto n = cc_ids.size();
  for (auto i = 0ul; i < n; ++i) {
    const auto& ccset = ids[i];

    std::cout << i << ": " << std::endl;
    print_cc_set(ccset);
    std::cout << std::endl;
  }

  std::cout << std::endl;
}

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

    const auto cc_copy = cc;
    for (const auto& cci : cc_copy) {
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
        if (temp.empty()) {
          continue;
        }

        // std::cout << "Transition from state " << cc_ids[cci] << " on symbol ";
        // print_symbol(x);
        // std::cout << std::endl;
        // print_cc_set(cci);

        if (!cc.count(temp)) {
          // std::cout << "to state " << id << ":" << std::endl;
          // print_cc_set(temp);
          // std::cout << std::endl;

          changing = true;
          cc.emplace(temp);

          cc_ids[temp] = id;
          ++id;
        } // else {
          // std::cout << "to existing state " << cc_ids[temp] << ":" << std::endl;
          // print_cc_set(temp);
          // std::cout << std::endl;
        // }

        // TODO: Record the transition from CCi to temp on symbol x.
      }
    }
  }
}


class Action {
public:
  enum class Type {
    NONE,
    SHIFT,
    REDUCE,
    ACCEPT
  };

  bool
  operator ==(const Action& other) const {
    return type == other.type &&
      arg == other.arg;
  }

  bool
  operator !=(const Action& other) const {
    return !operator==(other);
  }

  Type type = Type::NONE;
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
    const auto& item = rules[i];
    const auto& rule = item.production;
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
        const Action action = {Action::Type::SHIFT, j};

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
        const Action action = {Action::Type::ACCEPT, 0};

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
        const Action action = {Action::Type::REDUCE, rule_number};

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
        if (ccj.empty()) {
          // This is acceptable.
          // Not all parts of the goto table have entries.
          continue;
        }

        // If do_goto() generated a set, we should know about that set already:
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
