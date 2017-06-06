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

#ifndef MURRAYC_COMPILER_EXPERIMENTS_TREE_H
#define MURRAYC_COMPILER_EXPERIMENTS_TREE_H

#include <vector>
#include <cmath>

template <typename T_Value>
class TreeNode {
public:
  explicit TreeNode(const T_Value& value)
    : value_(value) {
    }

  ~TreeNode() {
    remove_all_children();
  }

  TreeNode* add(TreeNode* child) {
    child->parent_ = this;
    children_.emplace_back(child);
    return child;
  }

  TreeNode* add(const T_Value& value) {
    return add(new TreeNode(value));
  }

  T_Value& value() {
    return value_;
  }

  const T_Value& value() const {
    return value_;
  }

  TreeNode* leftmost_child() {
    if (children_.empty()) {
      return nullptr;
    }

    return children_[0];
  }

  const TreeNode* leftmost_child() const {
    if (children_.empty()) {
      return nullptr;
    }

    return children_[0];
  }

  // Remove the @a child from this parent.
  void remove_child(TreeNode* child) {
    for (auto iter = std::begin(children_); iter != std::end(children_); ++iter) {
      if (*iter == child) {
        children_.erase(iter);
        child->parent_ = nullptr;
        break;
      }
    }
  }

  /** Remove this from its parent,
   * and return the parent's next sibling, if any.
   */
  TreeNode* pop_to_parent() {
    auto parent = parent_;
    remove_all_children();
    delete this;
    return parent;
  }

  void print() {
    const auto max_depth = get_max_depth();

    for (std::size_t i = 0; i <= max_depth; ++i) {
      // The spacing should be 1, 3, 7, 15, etc, starting from the bottom.
      const auto indent = std::pow(2, max_depth - i + 1) - 1;
      print_indent(indent / 2);
      const auto printed = print_level(i, indent);
      std::cout << std::endl;
      if (!printed) {
        break;
      }
    }
  }

private:
  static void
  print_indent(std::size_t indent) {
    for (std::size_t i = 0; i < indent; ++i) {
      std::cout << ' ';
    }
  }

  /** Prints only the items at this exact @a depth.
   * @result Whether there were any nodes at that @a depth.
   */
  bool
  print_level(std::size_t depth, std::size_t indent) const {
    if (depth == 0) {
      std::cout << value_;
      print_indent(indent);
      return true;
    }

    bool result = false;
    for (const auto& child_node : children_) {
      if (!child_node) {
        // Print a space to keep the other ites aligned.
        // TODO: This assume that all items are a single character.
        std::cout << " ";
        print_indent(indent);
        continue;
      }

      if (child_node->print_level(depth - 1, indent)) {
        result = true;
      }
    }

    return result;
  }

  void remove_all_children() {
    for (auto child : children_) {
      delete child;
    }

    children_.clear();
  }

  std::size_t
  get_max_depth() const {
    std::size_t result = 0;

    for (const auto& child_node : children_) {
      if (!child_node) {
        continue;
      }

      const auto depth = child_node->get_max_depth();
      if (depth > result) {
        result = depth;
      }
    }

    return result + 1;
  }

  T_Value value_;
  TreeNode* parent_ = nullptr;
  std::vector<TreeNode*> children_;
};

#endif // MURRAYC_COMPILER_EXPERIMENTS_TREE_H
