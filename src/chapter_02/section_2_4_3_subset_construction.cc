#include <algorithm>
#include <iterator>
#include <map>
#include <memory>
#include <queue>
#include <string>
#include <unordered_map>
#include <set>
#include <vector>
#include <iostream>
#include <cassert>

class State {
public:
  using char_t = char;
  static constexpr char_t E = '#'; // Because no simple C++ char type an represent a multi-character literal for ε.

  explicit State(const std::string& id, bool accepting = false)
  : id_(id), accepting_(accepting) {
    assert(!id.empty());
  }

  /** Add a state from this state, via the character @a ch,
   * to a next @a state with the @a id.
   */
  std::shared_ptr<State> add(char_t ch, const std::shared_ptr<State>& state) {
    states_[ch].emplace(state);
    return state;
  }

  /** Add a state from this state, via the character @a ch,
   * to a new next state with the @a id.
   */
  std::shared_ptr<State>
  add(char_t ch, const std::string& id, bool accepting = false) {
    auto s = std::make_shared<State>(id, accepting);
    return add(ch, s);
  }

  /*
  std::vector<char_t>
  transitions() {
    std::vector<char_t> result;

    std::transform(std::begin(states_), std::end(states_),
      std::inserter(result, std::end(result)),
        [](const auto& pair){
          return pair.first;
        });

    return result;
  }
  */

  using States = std::set<std::shared_ptr<State>>;

  decltype(auto)
  next_states(char_t ch) {
    auto iter = states_.find(ch);
    if (iter == std::end(states_)) {
      return States();
    }

    return iter->second;
  }

  decltype(auto)
  next_states_via_e() {
    return next_states(State::E);
  }

  std::string id() const {
    return id_;
  }

  bool is_accepting() const {
    return accepting_;
  }

private:
  std::string id_;
  bool accepting_ = false;
  std::unordered_map<char_t, States> states_;
};

using States = State::States;

/// Get any states reachable from the @a states s via a transition on @a c.
static States
delta(const States& q, State::char_t c) {
  States result;

  for (const auto& s : q) {
    const auto sdest = s->next_states(c);
    if (sdest.empty()) {
      continue;
    }

    result.insert(std::begin(sdest), std::end(sdest));
  }

  return result;
}

/** Get any states reachable from the @a states via ε,
 * including the input @a states themselves.
 *
 * TODO: The "Computing ε-closure Offline" section has
 * pseudo-code to compute all the ε-closures at once,
 * but the "it places n's predecessors along ε-transitions
 * on the worklist" part is unclear. How are we supposed
 * to find the predecessors? Is it enough to use only
 * the predecessors discovered so far?
 */
static States
e_closure(const States& states) {
  // Start with the current states:
  States result = states;

  // Add any states that can be reached via ε transitions.
  for (const auto& s : states) {
    const auto sdest = s->next_states_via_e();
    if (sdest.empty()) {
      continue;
    }

    for (const auto& next : sdest) {
      if (result.count(next)) {
        continue;
      }

      result.emplace(next);

      // Recurse, to add other states reachable via a further ε transition.
      const auto further_states = e_closure({next});
      for (const auto& further : further_states) {
        result.emplace(further);
      }
    }
  }

  return result;
}

/*
static void
print_set(const States& states) {
  std::cout << "{";
  for (const auto& s : states) {
    std::cout << s->id() << ", ";
  }
  std::cout << "}";
}
*/

/** Subset construction.
 * Based on the pseudocode in Figure 2.6 on page 49.
 */
static decltype(auto)
subset_construction(const std::shared_ptr<State>& n0) {
  // Q begins with q0, which contains n0
  // and any states reachable via ε-only paths.
  auto q0 = e_closure({n0});
  std::set<States> Q;
  std::map<std::pair<States, char>, States> T;
  Q.emplace(q0);

  std::queue<States> worklist;
  worklist.emplace(q0);

  while (!worklist.empty()) {
    // Remove a set q from the worklist:
    // Each q represents a valid configuration of the original NFA.
    auto q = worklist.front();
    worklist.pop();

    // std::cout << "q" << i << std::endl;
    // std::cout << "  ";
    // print_set(q);
    // std::cout << std::endl;
    // ++i;

    // For each character in the alphabet,
    // construct the configuration that the NFA would reach if it read c
    // while in configuration q.
    // TODO: Performance: Check only actual transitions, not the whole alphabet.
    for (auto c = 'a'; c <= 'z'; ++c) {
      auto t = e_closure(delta(q, c));
      if (t.empty()) {
        continue;
      }

      // std::cout << "    " << c << ": ";
      // print_set(t);
      // std::cout << std::endl;

      const auto qckey = std::make_pair(q, c);
      T[qckey] = t;
      if (Q.count(t) == 0) {
        Q.emplace(t);
        worklist.emplace(t);
      }
    }
  }

  return std::make_pair(Q, T);
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

/** For each distinct set of states, qi, in @a Q, create a state di,
 * with the transitions mapped in @a T.
 *
 * Based on the description (without pseudocode) in the
 * "From Q to D" section on page 50.
 */
static decltype(auto)
from_q_to_d(const std::set<States>& Q, const std::map<std::pair<States, char>, States>& T) {
  // TODO: Performance: Using the set as a key is very inefficient.
  std::unordered_map<int, std::shared_ptr<State>> D;
  std::map<States, int> q_ids;
  int i = 0;
  for (const auto& qi : Q) {
    q_ids[qi] = i;

    const bool accepting = contains_accepting_state(qi);

    const std::string id = "d" + std::to_string(i);
    const auto di = std::make_shared<State>(id, accepting);
    D[i] = di;
    // std::cout << "q" << i << ": " << di->id() << std::endl;

    ++i;
  }

  // std::cout << std::endl;

  // For each state di (identified by its distinct set of states qi),
  // create a transition to another di, by matching the distinct set of states in q,
  // that the states qi have a transition to.
  for (const auto& ti : T) {
    const auto& [p, qdest] = ti;
    const auto& [srcset, c] = p;
    const auto qsrc_i = q_ids[srcset];
    const auto qdest_i = q_ids[qdest];

    // std::cout << "q" << qsrc_i << ": ";
    // print_set(srcset);
    // std::cout << ": " << c << ": ";
    // print_set(qdest);
    // std::cout << " (q" << qdest_i << ")" << std::endl;

    const auto& di = D[qsrc_i];
    const auto ddest = D[qdest_i];
    di->add(c, ddest);
  }

  return D;
}

/** Create a DFA from an NFA,
 * by identifying valid configurations
 * (sets of NFA states reachable from other sets of NFA states via transitions on particular characters)
 * and mapping them to DFA states.
 */
static std::shared_ptr<State>
nfa_to_dfa(const std::shared_ptr<State>& n0) {
  const auto [Q, T] = subset_construction(n0);
  const auto D = from_q_to_d(Q, T);

  if (D.empty()) {
    return nullptr;
  }

  // Return the first DFA state, d0:
  const auto iter = D.find(0);
  if (iter == std::end(D)) {
    return nullptr;
  }

  return iter->second;
}

/** Check that this DFA state has only one transition on this character,
 * and check that it is to the expected subsequent state.
 */
static std::shared_ptr<State>
check_only_transition(const std::shared_ptr<State>& state, State::char_t c, const std::string& dest_id) {
  assert(state);
  const auto dest_states = state->next_states(c);
  assert(dest_states.size() == 1);
  const auto dest = *(dest_states.begin());
  assert(dest->id() == dest_id);
  return dest;
}

int main() {
  {
    // The NFA from Figure 2.7 on page 51.
    auto n0 = std::make_shared<State>("n0");
    auto n2 = n0->add('a', "n1")
      ->add(State::E, "n2");
    auto n3 = n2->add(State::E, "n3");
    auto n5 = n3->add(State::E, "n4")
      ->add('b', "n5");
    auto n7 = n3->add(State::E, "n6")
      ->add('c', "n7");

    auto n8 = std::make_shared<State>("n8");
    auto n9 = n8->add(State::E, "n9", true);
    n2->add(State::E, n9);
    n8->add(State::E, n3);
    n5->add(State::E, n8);
    n7->add(State::E, n8);

    const auto d0 = nfa_to_dfa(n0);

    // The DFA from Figure 2.7 on page 51:
    assert(d0);
    assert(d0->id() == "d0");
    assert(!d0->is_accepting());

    const auto d1 = check_only_transition(d0, 'a', "d1");
    assert(d1->is_accepting());
    const auto d2 = check_only_transition(d1, 'b', "d2");
    assert(d2->is_accepting());
    check_only_transition(d2, 'b', "d2");
    check_only_transition(d2, 'c', "d3");
    const auto d3 = check_only_transition(d1, 'c', "d3");
    assert(d3->is_accepting());
    check_only_transition(d3, 'c', "d3");
    check_only_transition(d3, 'b', "d2");
  }

  return 0;
}
