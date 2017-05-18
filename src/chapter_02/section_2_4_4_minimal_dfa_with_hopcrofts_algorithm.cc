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

#include <iostream>
#include <map>
#include <cassert>

#include "state.h"

using States = State::States;
using Partitions = std::set<States>;

/**
 * Split the states into accepting states and non-accepting states.
 */
static Partitions
get_accepting_and_non_accepting_states(const States& states) {
  States accepting, non_accepting;
  for (const auto& s : states) {
    if (s->is_accepting()) {
      accepting.emplace(s);
    } else {
      non_accepting.emplace(s);
    }
  }

  return {accepting, non_accepting};
}

/** TODO: This is incredibly inefficient.
 */
static States
get_partition_containing_state(const Partitions& partitions, const std::shared_ptr<State>& s) {
  for (const auto partition : partitions) {
    if (partition.count(s)) {
      return partition;
    }
  }

  return {};
}

/** For each character,
 * check the states that can be transitioned to
 * from each of the states in the set of @a states.
 * If these transitioned-to states are not all in the same partition,
 * split them into separate sets of states according to the partitions that they belong to.
 * Otherwise return @a s.
 */
static Partitions
split(const States& states, const Partitions& partitions) {
  for (auto c = 'a'; c <= 'z'; ++c) {

    // Map of partitions to states that transition to those partitions.
    std::map<States, States> states_by_next_partition;

    for (const auto& s : states) {
      const auto next_states = s->next_states(c);

      // We don't ignore empty next_states, because states with no transition on c
      // should be in their own partition, according to page 56.
      // (This also ensures that all the sets will be in the result.)

      // DFAs, by definition, have at most 1 transition per character per state:
      assert(next_states.size() <= 1);

      const auto next = *(next_states.begin());
      const auto next_partition = get_partition_containing_state(partitions, next);
      states_by_next_partition[next_partition].emplace(s);
    }

    // If the next states are in more than one partition,
    // return the states that lead to the partitions:
    // TODO: The example on page 56 splits off just one partition each time,
    // but in step 2, this would split {s0, s1, s2, s4} into {{s0}, {s1}, {s2, s4}},
    // instead of the {{s0, s1}, {s2, s4} shown in the example.
    // That seems to be OK.
    if (states_by_next_partition.size() > 1) {
      Partitions result;
      for (const auto& pair : states_by_next_partition) {
        result.emplace(pair.second);
      }

      return result;
    }
  }

  return {states};
}

static void
print_set(const States& states) {
  bool first = true;
  std::cout << "{";
  for (const auto& s : states) {
    if (!first) {
      std::cout << ", ";
    }

    std::cout << s->id();
    first = false;
  }
  std::cout << "}";
}

static void
print_partitions(const Partitions& partitions) {
  bool first = true;
  std::cout << "{";
  for (const auto& states : partitions) {
    if (!first) {
      std::cout << ", ";
    }

    print_set(states);
    first = false;
  }
  std::cout << "}";
}

static Partitions
construct_partitions(const States& states) {
  Partitions T = get_accepting_and_non_accepting_states(states);
  Partitions P;

  std::cout << "Current partitions: ";
  print_partitions(T);
  std::cout << std::endl;

  while (P != T) {
    P = T;
    T.clear();

    for (const auto& p : P) {
      std::cout << "  Splitting on partition: ";
      print_set(p);
      std::cout << std::endl;

      const auto s = split(p, P);
      std::cout << "    Result: ";
      print_partitions(s);
      std::cout << std::endl;

      T.insert(std::begin(s), std::end(s));
    }

    std::cout << "Current partitions: ";
    print_partitions(T);
    std::cout << std::endl;
  }

  return P;
}

/**
 * TODO: Performance: Store this as part of the States.
 */
static bool
contains_accepting_state(const States& states) {
  for (const auto& s: states) {
    if (s && s->is_accepting()) {
      return true;
    }
  }

  return false;
}

/**
 * Return the minimized states, by using the @a partitions,
 * and return the state that corresponds to a partition with the original state @a s0.
 */
static std::pair<States, std::shared_ptr<State>>
construct_states_from_partitions(const Partitions& partitions, const std::shared_ptr<State>& s0) {
  States result;
  std::shared_ptr<State> minimized_s0;

  int i = 0;
  std::map<int, std::shared_ptr<State>> D;
  std::map<States, int> states_by_partition;
  for (const auto& p : partitions) {
    if (p.empty()) {
      continue;
    }

    const auto id = "d" + std::to_string(i);

    // TODO: The text doesn't say that this is how we should decide
    // if the state in the minimized DFA should be accepting,
    // but it seems to be appropriate and works for the example.
    const auto accepting = contains_accepting_state(p);
    const auto di = std::make_shared<State>(id, accepting);
    D[i] = di;
    states_by_partition[p] = i;
    result.emplace(di);

    if (p.count(s0)) {
      minimized_s0 = di;
    }

    ++i;
  }

  i = 0;
  for (const auto& p : partitions) {
    if (p.empty()) {
      continue;
    }

    auto di = D[i];

    // TODO: This is horribly inefficient:
    States next_partition;
    std::set<State::char_t> next_partition_chars;
    for (const auto s : p) {
      const auto [next_chars, next_states] = s->next_states();
      next_partition.insert(std::begin(next_states), std::end(next_states));
      next_partition_chars.insert(std::begin(next_chars), std::end(next_chars));
    }

    const auto iter = states_by_partition.find(next_partition);
    if (iter != std::end(states_by_partition)) {
      const auto nexti = iter->second;
      auto nextd = D[nexti];

      // Add transitions to the new states that represent the transitioned-to partitions.
      // (This part isn't really described in the book's test.)
      //
      // TODO: Allow one transition to have multiple characters,
      // as in the diagrams in the book?
      for (const auto c : next_partition_chars) {
        std::cout << di->id() << ": adding transition (" << c << ") to " << nextd->id() << std::endl;
        di->add(c, nextd);
      }
    }

    ++i;
  }

  return {result, minimized_s0};
}

static std::shared_ptr<State>
construct_minimal_dfa(const std::shared_ptr<State>& s0, const States& states) {
  const auto partitions = construct_partitions(states);
  const auto [minimized_states, minimized_s0] = construct_states_from_partitions(partitions, s0);

  return minimized_s0;
}

int main() {
  {
    // The DFA from Figure 2.7 on page 51,
    // and Figure 2.11 (a) on page 56:
    auto s0 = std::make_shared<State>("s0");
    auto s1 = s0->add('f', "s1");

    auto s2 = s1->add('e', "s2");
    auto s3 = s2->add('e', "s3", true);

    auto s4 = s1->add('i', "s4");
    auto s5 = s4->add('e', "s5", true);

    const States states = {s0, s1, s2, s3, s4, s5};
    auto s0_minimized = construct_minimal_dfa(s0, states);

    // The minimized DFA from Figure 2.11 (b) on page 56:
    assert(s0_minimized);
    assert(s0_minimized->id() == "d0");
    assert(s0_minimized->count_transitions() == 1);

    auto next = s0_minimized->next_states('f');
    assert(next.size() == 1);

    auto s1_minimized = *(next.begin());
    assert(s1_minimized);
    assert(s1_minimized->id() == "d1");
    assert(!s1_minimized->is_accepting());
    assert(s1_minimized->count_transitions() == 2); // 1 for i and 1 for e.
    next = s1_minimized->next_states('i');
    assert(next.size() == 1);
    next = s1_minimized->next_states('e');
    assert(next.size() == 1);

    auto s2_minimized = *(next.begin());
    assert(s2_minimized);
    assert(s2_minimized->id() == "d2");
    assert(!s2_minimized->is_accepting());
    assert(s2_minimized->count_transitions() == 1);
    next = s2_minimized->next_states('e');
    assert(next.size() == 1);

    auto s3_minimized = *(next.begin());
    assert(s3_minimized);
    assert(s3_minimized->id() == "d3");
    assert(s3_minimized->is_accepting());
  }

  // TODO: Test the second example too.

  return 0;
}
