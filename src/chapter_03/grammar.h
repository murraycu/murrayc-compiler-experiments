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

#ifndef MURRAYC_COMPILER_EXPERIMENTS_GRAMMARS_H
#define MURRAYC_COMPILER_EXPERIMENTS_GRAMMARS_H

#include "symbol.h"
#include <map>
#include <unordered_map>
#include <vector>

// A set of possible expansions.
using Expansions = std::vector<Symbols>;

// A set of rules (also known as productions, or productin rules), mapping a
// symbol to its possible expansions.  We could instead just have a flat set of
// rules (in a multimap), with more than one with the same left symbol.  That
// might be what the pseudo code in Figure 3.2 is meant to use.
using GrammarRules = std::map<Symbol, Expansions>;

using WordsMap = std::map<std::string, Symbol>;

// The "concept" for grammar classes:
// Grammar {
//  static const Symbols symbols;
//  static const GrammarRules rules;
//
//  static const Symbol SYMBOL_GOAL;
//  static const Symbol SYMBOL_EOF;
//  static const Symbol SYMBOL_EMPTY;
//
//  static Symbol
//  recognise_word(const WordsMap& words_map, const std::string& word);
//
//  static WordsMap
//  build_words_map();
// };

#endif // MURRAYC_COMPILER_EXPERIMENTS_GRAMMARS_H
