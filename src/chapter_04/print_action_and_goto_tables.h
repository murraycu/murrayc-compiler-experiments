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

#ifndef MURRAYC_COMPILER_EXPERIMENTS_PRINT_ACTION_AND_GOTO_TABLES_H
#define MURRAYC_COMPILER_EXPERIMENTS_PRINT_ACTION_AND_GOTO_TABLES_H

#include "build_action_and_goto_tables.h"
#include <iostream>

template <typename T_Value>
void
print_action(const Action& action, const GrammarRulesByNumber<T_Value>& rules) {
  std::string str;
  switch (action.type) {
    case Action::Type::SHIFT:
      str = "s";
      break;
    case Action::Type::REDUCE:
      str = "r";
      break;
    case Action::Type::ACCEPT:
      str = "a";
      break;
    default:
      str = "?";
      break;
  }

  std::cout << str << action.arg;

  if (action.type == Action::Type::REDUCE) {
    std::cout << " (";
    const auto& item = rules.at(action.arg);
    print_rule(rules.at(item.first));
    std::cout << ")";
  }
}

template <typename T_Grammar>
void
print_action_and_goto_tables(const ActionTable& action_table, const GotoTable& goto_table) {
  std::cout << "Rules:" << std::endl;
  const auto& rules = rules_by_number<T_Grammar>();
  std::size_t i = 0;
  for (const auto& rule : rules) {
    std::cout << i << ": ";
    print_rule(rule);
    std::cout << std::endl;

    ++i;
  }
  std::cout << std::endl;

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
      print_action(action, rules);
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
}

#endif // MURRAYC_COMPILER_EXPERIMENTS_PRINT_ACTION_AND_GOTO_TABLES_H
