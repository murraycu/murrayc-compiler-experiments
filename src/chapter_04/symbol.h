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

#ifndef MURRAYC_COMPILER_EXPERIMENTS_SYMBOL_H
#define MURRAYC_COMPILER_EXPERIMENTS_SYMBOL_H

#include <iostream>
#include <string>
#include <vector>
#include <cstring>

// An enum would be more efficient.
class Symbol {
public:
  bool operator==(const Symbol& other) const {
    if (!name) {
      return !(other.name);
    } else if (!(other.name)) {
      return !name;
    }

    if (name == other.name) {
      return true;
    }

    return strcmp(name, other.name) == 0;
  }

  bool operator!=(const Symbol& other) const {
    return !(operator==(other));
  }

  bool operator<(const Symbol& other) const {
    // Sort null before non-null,
    // though we don't expect nulls anyway.
    if (name && !(other.name)) {
      return true;
    }

    if (!name && other.name) {
      return false;
    }

    if (name == other.name) {
      return terminal < other.terminal;
    }

    const auto c = strcmp(name, other.name);
    if (c == 0) {
      return terminal < other.terminal;
    } else {
      return c < 0;
    }
  }

  // TODO: Use std::string_view
  const char* name;

  bool terminal = false;
};

// A set of symbols, such as a possible expansion, or a full parse.

using Symbols = std::vector<Symbol>;

void
print_symbol(const Symbol& symbol) {
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

#endif // MURRAYC_COMPILER_EXPERIMENTS_SYMBOL_H
