#include <set>
#include <stack>
#include <string>
#include <unordered_map>
#include <vector>
#include <cassert>

constexpr const char* STATE_BAD = "bad";
constexpr const char* STATE_END = "se";
constexpr const char CHAR_EOF = '$'; //arbitrary value.

template <typename T_Element>
static void
clear_stack(std::stack<T_Element>& st) {
  while (!st.empty()) {
    st.pop();
  }
}

using TokenType = std::string;
constexpr const char* TOKEN_TYPE_INVALID = "invalid";

/** Get the first lexeme, and its type, from the string,
 * by scanning with the DFA defined by the provided tables.
 *
 * Based on the pseudocode in Figure 2.14, in section 2.5.1,
 * of "Engineering a compiler".
 *
 * @param str The string to scan.
 * @param states The set of all states, not including the end state.
 * @param s0 The starting state.
 * @param CharCat The classifier table, mapping character to categories.
 * @param Delta The transition table, mapping states and categories to subsequent states.
 * @param Type The token type table, mapping accepting states to token types.
 * the classifier table @a CharCat,
 * the .
 */
template <typename T_StateID, typename T_CharCat, typename T_Delta, typename T_Type>
static std::pair<TokenType, std::string>
scan(const std::string& str, const std::set<T_StateID>& states, const T_StateID& s0, const T_CharCat& CharCat, const T_Delta& Delta, const T_Type& Type) {
  auto state = s0;
  std::string lexeme;

  auto istr = str.begin();

  std::stack<T_StateID> st;
  st.emplace(STATE_BAD);

  while (state != STATE_END) {
    const auto ch = (istr == std::end(str) ? CHAR_EOF : *istr);
    ++istr;

    lexeme += ch;

    if (states.count(state)) {
      clear_stack(st);
    }
    st.push(state);

    const auto itercat = CharCat.find(ch);
    if (itercat == std::end(CharCat)) {
      return {lexeme, TOKEN_TYPE_INVALID};
    }
    const auto cat = itercat->second;

    const auto iterdelta = Delta.find(state);
    if (iterdelta == std::end(Delta)) {
      return {lexeme, TOKEN_TYPE_INVALID};
    }
    const auto& transitions = iterdelta->second;

    const auto iter = transitions.find(cat);
    if (iter == std::end(transitions)) {
      return {lexeme, TOKEN_TYPE_INVALID};
    }

    state = iter->second;
  }

  // If we have gone too far, repeatedly roll back one character and one state,
  // until we are back to a useful state:
  //
  // For instance, when we hit the end state,
  // we must roll back once.
  while (!states.count(state) &&
    state != STATE_BAD) {
    state = st.top();
    st.pop();

    // truncate the lexeme:
    lexeme.erase(lexeme.size() - 1, std::string::npos);

    // rollback the input stream
    --istr;
  }

  if (states.count(state)) {
    return {lexeme, Type.at(state)};
  } else {
    return {lexeme, TOKEN_TYPE_INVALID};
  }
}

/** Get the first lexeme, and its type, from the string @a str.
 */
static std::pair<std::string, std::string>
recognise_register(const std::string& str) {
  enum class Categories {
    REGISTER,
    DIGIT,
    OTHER
  };

  // These tables are from Figure 2.14, in section 2.5.1,
  // of "Engineering a compiler".
  const std::unordered_map<char, Categories> classifier_table = {
    {'r', Categories::REGISTER},
    {'0', Categories::DIGIT},
    {'1', Categories::DIGIT},
    {'2', Categories::DIGIT},
    {'3', Categories::DIGIT},
    {'4', Categories::DIGIT},
    {'5', Categories::DIGIT},
    {'6', Categories::DIGIT},
    {'7', Categories::DIGIT},
    {'8', Categories::DIGIT},
    {'9', Categories::DIGIT},
    {CHAR_EOF, {Categories::OTHER}}};

  // The set of all states,
  // not including the end state:
  const std::set<std::string> states = {"s0", "s1", "s2", "s3"};

  // Map of states to map-of-categories-to-states:
  const std::unordered_map<std::string, std::unordered_map<Categories, std::string>> transition_table = {
    {"s0", {
      {Categories::REGISTER, "s1"},
      {Categories::DIGIT, STATE_END},
      {Categories::OTHER, STATE_END}}},
    {"s1", {
      {Categories::REGISTER, STATE_END},
      {Categories::DIGIT, "s2"},
      {Categories::OTHER, STATE_END}}},
    {"s2", {
      {Categories::REGISTER, STATE_END},
      {Categories::DIGIT, "s2"},
      {Categories::OTHER, STATE_END}}},
    {"s3", {
      {Categories::REGISTER, STATE_END},
      {Categories::DIGIT, STATE_END},
      {Categories::OTHER, STATE_END}}}};

  const std::unordered_map<std::string, TokenType> token_type_table = {
    {"s0", TOKEN_TYPE_INVALID},
    {"s1", TOKEN_TYPE_INVALID},
    {"s2", "register"},
    {"s3", TOKEN_TYPE_INVALID}};

  return scan(str, states, std::string("s0"), classifier_table, transition_table, token_type_table);
}

int main() {
  {
    assert(recognise_register("r2").second == "register");
    assert(recognise_register("r9").second == "register");
    assert(recognise_register("x2").second == TOKEN_TYPE_INVALID);
    assert(recognise_register("r").second == TOKEN_TYPE_INVALID);
  }

  return 0;
}
