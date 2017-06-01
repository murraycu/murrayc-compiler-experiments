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

#ifndef MURRAYC_COMPILER_EXPERIMENTS_BUILD_SETS_H
#define MURRAYC_COMPILER_EXPERIMENTS_BUILD_SETS_H

#include "symbol.h"
#include <algorithm>
#include <iostream>
#include <set>
#include <map>
#include <cassert>

template <typename T_Container, typename T_Value>
static bool
contains(const T_Container& symbols, const T_Value& symbol) {
  return std::find(std::begin(symbols), std::end(symbols), symbol) !=
    std::end(symbols);
}

template <typename T_Container, typename T_Value>
static T_Container
remove_symbol(const T_Container& symbols, const T_Value& symbol) {
  auto result = symbols;

  auto iter = result.find(symbol);
  if (iter != std::end(result)) {
    result.erase(iter);
  }

  /*
  result.erase(
    std::remove(std::begin(result), std::end(result), symbol),
    std::end(result));
  */
  return result;
}

using SymbolSet = std::set<Symbol>;
using FirstSets = std::map<Symbol, SymbolSet>;

/** Compute the FIRST sets for symbols in a grammar.
 * A FIRST(symbol) set is the set of terminals that can appear at the start of a sentence derived from the symbol.
 *
 * Based on the pseudocode in Figure 3.7, in section 3.3.1, on page 104
 * of "Engineering a Compiler".
 */
template <typename T_Grammar>
static FirstSets
build_first_sets() {

  FirstSets first;

  {
    // The pseudo code in Figure 3.7,
    // says we should do this on the union of the terminals, eof, and E.
    SymbolSet symbols(std::begin(T_Grammar::symbols), std::end(T_Grammar::symbols));
    symbols.emplace(T_Grammar::SYMBOL_EMPTY);
    assert(contains(symbols, T_Grammar::SYMBOL_EOF));

    for (const auto& symbol : symbols) {
      if (symbol.terminal) {
        const SymbolSet temp = {symbol};
        first[symbol] = temp;
        assert(first[symbol] == temp);
        assert(first[symbol].size() == 1);
      } else {
        first[symbol] = {};
      }
    }
  }

  const auto symbols = T_Grammar::symbols;
  const auto& rules = T_Grammar::rules;
  assert(!contains(symbols, T_Grammar::SYMBOL_EMPTY));

  for (const auto& symbol : symbols) {
    if (symbol.terminal) {
      if (first[symbol].size() != 1) {
        std::cerr << "symbol: " << symbol.name << ", first[any terminal] should = the terminal." << std::endl;
      }

      assert(first[symbol].size() == 1);
    }
  }

  bool changing = true;
  while (changing) {
    changing = false;

    for (const auto& rule : rules) {
      const auto& a = rule.first;
      const auto& expansions = rule.second;
      for (const auto& b : expansions) {
        const auto k = b.size();
        if (k == 0) {
          continue;
        }

        const auto& b1 = b[0];
        auto rhs = remove_symbol(first[b1], T_Grammar::SYMBOL_EMPTY);

        // The pseudocode uses 1-indexing, but we use 0-indexing.
        std::size_t i = 0;
        while (contains(first[b[i]], T_Grammar::SYMBOL_EMPTY) && i < (k - 1)) {
          const auto& bip1 = b[i+1];
          const auto more = remove_symbol(first[bip1], T_Grammar::SYMBOL_EMPTY);
          rhs.insert(std::begin(more), std::end(more));
          ++i;
        }

        const auto& bk = b[k-1];
        if (i == (k - 1) && contains(first[bk], T_Grammar::SYMBOL_EMPTY)) {
          rhs.emplace(T_Grammar::SYMBOL_EMPTY);
        }

        auto& firsta = first[a];
        const auto count_before = firsta.size();
        firsta.insert(std::begin(rhs), std::end(rhs));
        changing |= (count_before != firsta.size());
      }
    }
  }

  return first;
}

using FollowSets = std::map<Symbol, SymbolSet>;

/** Compute the FOLLOWS sets for symbols in a grammar.
 * A FOLLOWS(Symbol) set is the set of words that can occur immediately after the (nonterminal) symbol in a sentence.
 *
 * Based on the pseudocode in Figure 3.8, in section 3.3.1, on page 106
 * of "Engineering a Compiler".
 */
template <typename T_Grammar>
static FollowSets
build_follow_sets(const FirstSets& first) {
  const auto& rules = T_Grammar::rules;

  FollowSets follow;
  // std::cout << "build_follow_sets:" << std::endl;

  // The pseudocode has this:
  // FOLLOW(S) <- {eof}
  // but doesn't say what S is.
  // However, the earlier pseudocode, in Figure 3.2 on page 98 does refer to S as the start symbol.
  // TODO: Submit as Errata?
  follow[T_Grammar::SYMBOL_GOAL] = {T_Grammar::SYMBOL_EOF};

  bool changing = true;
  while (changing) {
    // std::cout << std::endl;
    // std::cout << "  loop start:" << std::endl;

    changing = false;

    for (const auto& rule : rules) {
      const auto& a = rule.first;
      const auto& expansions = rule.second;
      for (const auto& b : expansions) {
        // std::cout << std::endl;
        // std::cout << "    rule: " << a.name << " -> ";
        // print_symbols(b);
        // std::cout << std::endl;

        auto trailer = follow[a];
        // std::cout << "    trailer (set to follow[" << a.name << "]): ";
        // print_symbols(trailer);
        // std::cout << std::endl;

        const auto k = b.size();
        for (int i = k - 1; i >= 0; --i) {
          const auto& bi = b[i];
          const auto iter = first.find(bi);
          const auto firstbi = (iter == std::end(first)) ? SymbolSet() : iter->second;
          // std::cout << "      bi: " << bi.name << std::endl;
          // std::cout << "      first[" << bi.name << "]: ";
          // print_symbols(firstbi);
          // std::cout << std::endl;

          if (!(bi.terminal)) {
            auto& followbi = follow[bi];
            // std::cout << "        FOLLOW[" << bi.name << "] (initially): ";
            // print_symbols(followbi);
            // std::cout << std::endl;

            // The FOLLOW for this right-hand side symbol is based on the FIRST for
            // the right-hand side symbol further to the right (which we previously put in trailer):
            const auto old_count = followbi.size();
            followbi.insert(std::begin(trailer), std::end(trailer));
            changing |= (old_count != followbi.size());
            // std::cout << "        FOLLOW[" << bi.name << "] (after adding trailer): ";
            // print_symbols(followbi);
            // std::cout << std::endl;

            if (contains(firstbi, T_Grammar::SYMBOL_EMPTY)) {
              const auto firstbi_minus_e = remove_symbol(firstbi, T_Grammar::SYMBOL_EMPTY);
              trailer.insert(std::begin(firstbi_minus_e), std::end(firstbi_minus_e));
              // std::cout << "        trailer (after adding (bi - e)): ";
              // print_symbols(trailer);
              // std::cout << std::endl;
            } else {
              trailer = firstbi;
              // std::cout << "        trailer (after setting to bi, for non-terminal): ";
              // print_symbols(trailer);
              // std::cout << std::endl;
            }
          } else {
            // Note: FIRST[bi] for a terminal will always be just {bi}.
            trailer = firstbi;
            // std::cout << "        trailer (after setting to bi, for terminal): ";
            // print_symbols(trailer);
            // std::cout << std::endl;
          }
        }
      }
    }
  }

  return follow;
}

/** A left-hand side symbol and a possible right-hand side expansion.
 */
using GrammarRule = std::pair<Symbol, Symbols>; // GrammarRules::value_type;

/** Compute the FIRST sets for a sequence of symbols in a grammar.
 * (FIRST sets are usually for individual symbols.)
 * Union of FIRST(symbol) for b1 b2 ... bn, where bn is the first symbol whose FIRST(symbol) does not contain the empty symbol ε.
 *
 * Based on the definition in section 3.3.1, on page 105
 * of "Engineering a Compiler".
 */
template <typename T_Grammar>
static SymbolSet
build_first_set_for_symbols(const FirstSets& first, const Symbols& symbols) {
  SymbolSet result;

  // Page 105 says "We define FIRST sets over single grammar symbols
  // ..., For a string [set] of symbols, s = B1 B2 B3...Bn, we
  // define FIRST(s) as the union of the FIRST sets for B1, B2...Bn,
  // where Bn is the first symbol whose FIRST set does not contain E,
  // and E".

  for (const auto bi : symbols) {
    assert(first.count(bi));
    const auto& firstbi = first.at(bi);

    result.insert(std::begin(firstbi), std::end(firstbi));

    // Stop after the first symbols whose FIRST set does not contain E
    if (!contains(firstbi, T_Grammar::SYMBOL_EMPTY)) {
      break;
    }
  }

  // Make sure that the result contains E if, and only if, every FIRST(symbol) contains E,
  // as per the description on page 105 of "Engineering a Compiler".
  bool every_first_has_e = true;
  for (const auto bi : symbols) {
    assert(first.count(bi));
    const auto& firstbi = first.at(bi);
    if (!contains(firstbi, T_Grammar::SYMBOL_EMPTY)) {
      every_first_has_e = false;
      break;
    }
  }

  if (every_first_has_e) {
    // Add it:
    result.emplace(T_Grammar::SYMBOL_EMPTY);
  } else {
    // Remove it:
    const auto iter = result.find(T_Grammar::SYMBOL_EMPTY);
    if (iter != std::end(result)) {
      result.erase(iter);
    }
  }

  return result;
}

using FirstSetsForRules = std::map<GrammarRule, SymbolSet>;

/** Compute the FIRST sets for rules in a grammar.
 * (FIRST sets are usually for individual symbols.)
 * See build_first_set_for_symbols().
 *
 * Based on the definition in section 3.3.1, on page 105
 * of "Engineering a Compiler".
 */
template <typename T_Grammar>
static FirstSetsForRules
build_first_sets_for_rules(const FirstSets& first) {
  FirstSetsForRules result;

  const auto& rules = T_Grammar::rules;
  for (const auto& rule : rules) {
    const auto& a = rule.first;
    const auto& expansions = rule.second;
    for (const auto& b : expansions) {
      const auto firstb = build_first_set_for_symbols<T_Grammar>(first, b);
      const auto single_rule= std::make_pair(a, b);
      result[single_rule] = firstb;
    }
  }

  return result;
}

using FirstPlusSets = std::map<GrammarRule, SymbolSet>;

/** Compute the FIRST+ sets for symbols in a grammar.
 * A FIRST+(rule) set is FIRST(production), if that does not contain the empty symbol ε.
 * Otherwise, it is the union of FIRST(production) and FOLLOW(a).
 *
 * Based on the definition in section 3.3.1, on page 107
 * of "Engineering a Compiler".
 */
template <typename T_Grammar>
static FirstPlusSets
build_first_plus_sets(const FirstSetsForRules& first, const FollowSets& follow) {
  FirstPlusSets first_plus;

  const auto& rules = T_Grammar::rules;
  for (const auto& rule : rules) {
    const auto& a = rule.first;
    const auto& expansions = rule.second;
    for (const auto& b : expansions) {
      const auto single_rule= std::make_pair(a, b);

      // Get FIRST(b):
      SymbolSet firstb;
      const auto iter = first.find(single_rule);
      if (iter != std::end(first)) {
        firstb = iter->second;
      }

      // std::cout << "FIRST[" << a.name << " -> ";
      // print_symbols(b);
      // std::cout << "]: ";
      // print_symbols(firstb);
      // std::cout << std::endl;

      SymbolSet set = firstb;
      if (contains(firstb, T_Grammar::SYMBOL_EMPTY)) {
        const auto fiter = follow.find(a);
        if (fiter != std::end(follow)) {
          const auto& followa = fiter->second;
          set.insert(std::begin(followa), std::end(followa));
        }
      }

      first_plus[single_rule] = set;

      // std::cout << "FIRST+[" << a.name << " -> ";
      // print_symbols(b);
      // std::cout << "]: ";
      // print_symbols(set);
      // std::cout << std::endl;
    }

    // std::cout << std::endl;
  }

  return first_plus;
}

#endif // MURRAYC_COMPILER_EXPERIMENTS_BUILD_SETS_H
