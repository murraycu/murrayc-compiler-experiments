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

#include <string>

// An enum would be more efficient.
class Symbol {
public:
  bool operator==(const Symbol& other) const {
    return name == other.name &&
      terminal == other.terminal;
  }

  bool operator<(const Symbol& other) const {
    if (name == other.name) {
      return terminal < other.terminal;
    }

    return name < other.name;
  }

  std::string name;
  bool terminal = false;
};

#endif // MURRAYC_COMPILER_EXPERIMENTS_SYMBOL_H
