#ifndef PQ_H
#define PQ_H

#include "dsexceptions.h"
#include <algorithm>
#include <iostream>
#include <utility> // for make_pair()
#include <vector>
using namespace std;

// PQ class
//
// Template parameter: ID
// Constructors:
// PQ --> constructs a new empty queue
// PQ( tasks, priorities ) --> constructs a new queue with a given set of task IDs and priorities
// ******************PUBLIC OPERATIONS*********************
// void insert( x, p )       --> Insert task ID x with priority p
// ID findMin( )  --> Return a task ID with smallest priority, without removing it
// ID deleteMin( )   --> Remove and return a task ID with smallest priority
// void updatePriority( x, p )   --> Changes priority of ID x to p (if x not in PQ, inserts x);
// bool isEmpty( )   --> Return true if empty; else false
// int size() --> return the number of task IDs in the queue
// void makeEmpty( )  --> Remove all task IDs (and their priorities)
//
// Added:
// void print_all( )  --> Print tree and heap for debugging use
//
// ******************ERRORS********************************
// Throws UnderflowException as warranted

template <typename ID>
// ID is the type of task IDs to be used; the type must be Comparable (i.e.,
// have < defined), so IDs can be AVL Tree keys.
class PQ {

public:
  // Constructor
  // Initializes a new empty PQ
  PQ()
  // capacity = 101 pairs; each pair has two elements
  : array(101), currentSize{0}, root{nullptr}, recentlyDeleted{-1} {}

  // Constructor
  // Initializes a new PQ with a given set of tasks IDs and priorities
  //      priority[i] is the priority for ID task[i]
  PQ(const std::vector<ID> &tasks, const std::vector<int> &priorities)
      : array(tasks.size() + 10), currentSize{tasks.size()}, root{nullptr} {
    for (int i = 0; i < tasks.size(); i++) {
      insert(tasks[i], root, -1); // insert task ID and index in heap
      array[i + 1] = make_pair(priorities[i], findNode(tasks[i],root));
    }
    buildHeap();
    for (int i = 1; i <= tasks.size(); i++) {
      array[i].second->index = i;
    }
  }

  // Emptiness check
  bool isEmpty() const { return currentSize == 0; }

  // Deletes and Returns a task ID with minimum priority
  //    Throws exception if queue is empty
  const ID &deleteMin() {
    if (isEmpty())
      throw UnderflowException{};
    
    // temporarily store a copy of min for return value
    recentlyDeleted = findMin();
    // Swap heap bottom top, percolate down
    array[1] = std::move(array[currentSize--]);

    percolateDown(1);

    // Update the heap index in AVL nodes
    for (int i = 1; i <= currentSize; i++) {
      (array[i].second)->index = i;
    }

    // Remove AVL node containing the task to be deleted, balance tree
    remove(recentlyDeleted, root);

    // Using index in AVL nodes, update heap ptr address
    traverseTreeIndex(root);

    return recentlyDeleted;
  }

  // Returns an ID with minimum priority without removing it
  //     Throws exception if queue is empty
  const ID &findMin() const {
    if (isEmpty())
      throw UnderflowException{};
    return array[1].second->element;
  }

  // Insert ID x with priority p.
  void insert(const ID &x, int p) {
    insert(x, root, -1); // index unknown yet
    AvlNode *ptr;
    ptr = findNode(x, root);

    if (currentSize == array.size() - 1)
      array.resize(array.size() * 2);

    // Percolate up
    int hole = ++currentSize;
    pair<int, AvlNode *> tmp = make_pair(p, ptr);
    pair<int, AvlNode *> copy = tmp;

    array[0] = std::move(copy);
    // if insertion's p < its parent's p (if exists)
    // copy parent down (down = up)
    // copy new child's index
    // copy insertion to up
    for (; tmp.first < array[hole / 2].first; hole /= 2){
      array[hole] = std::move(array[hole / 2]);
      array[hole].second->index = hole;
    }
    array[hole] = std::move(array[0]);
    
    array[hole].second->index = hole; // Stamp index
  }

  // Update the priority of ID x to p
  //    Inserts x with p if s not already in the queue
  void updatePriority(const ID &x, int p) {

    AvlNode *ptr = findNode(x, root);
    if (ptr != nullptr) {               // If exist
      int old_p = array[ptr->index].first; // store old p
      array[ptr->index].first = p;         // update priority
      if (old_p > p) {                  // if decreased # priority, percolate up
        int old_index = ptr->index;
        // if not the top heap node & changed p < its parent's p
        for (; old_index > 1 && p < array[old_index / 2].first; old_index /= 2)
          array[old_index] = std::move(array[old_index / 2]);
        array[old_index].first = p;
        array[old_index].second = ptr;
      } else if (old_p < p) { // percolate down
        int old_index = ptr->index;
        percolateDown(old_index);
      } else { // changed nothing, do nothing
      }
      for (int i = 1; i <= currentSize; i++) // stamp index
        array[i].second->index = i;
    }
    // insert new
    else
      insert(x, p);
  }

  // Return the number of task IDs in the queue
  int size() const { return currentSize; }

  // Delete all IDs from the PQ
  void makeEmpty() {
    makeEmpty(root);
    currentSize = 0;
  }

  void print_all(ostream &out = cout) const {
    if (isEmpty())
      cout << "\nEmpty PQ" << endl;
    else {
      cout << "\n[ Printing all... ]\n\nThe current size is " << currentSize << endl;
      cout << "The task ID with smallest priority is " << findMin() << endl;
      cout << "\n[ Printing AVL Tree (Task ID,Heap Index) ]" << endl;
      printTree(root);
      cout << "\n[ Display AVL Tree w/ Address ]" << endl;
      displayLinks(root,0,out);
      cout << "\n[ Printing Heap (Priority,AVL Pointer) ]" << endl;
      cout << "Array = {\n0:\t(n/a,n/a)\n";
      for (int i = 1; i <= currentSize; i++) {
        cout << i << ":\t(" << array[i].first << "," << array[i].second << ")" << endl;
      }
      cout << "}\n" << endl;
    }
  }

private:
  // AVL tree
  struct AvlNode {
    ID element; // task ID
    int index;  // heap index
    AvlNode *left;
    AvlNode *right;
    int height;

    AvlNode(const ID &ele, int i, AvlNode *lt, AvlNode *rt, int h = 0)
        : element{ele}, index{i}, left{lt}, right{rt}, height{h} {}

    AvlNode(ID &&ele, int i, AvlNode *lt, AvlNode *rt, int h = 0)
        : element{std::move(ele)}, index{i}, left{lt}, right{rt}, height{h} {}
  };

  AvlNode *root;
  

  // Heap
  // Pair vector stores priority and for task t and a pointer to tree node for t
  int currentSize;                    // Number of elements in heap
  vector<pair<int, AvlNode *>> array; // The heap array
  ID recentlyDeleted;                 // Keep a copy of recently deleted (for extract_min)

  /**
   * Internal method to insert into a subtree.
   * x is the task to insert.
   * t is the node that roots the subtree.
   * i is the priority of x
   * Set the new root of the subtree.
   */
  void insert(const ID &x, AvlNode *&t, int i) {
    if (t == nullptr)
      t = new AvlNode{x, i, nullptr, nullptr};
    else if (x < t->element)
      insert(x, t->left, i);
    else if (t->element < x)
      insert(x, t->right, i);

    balance(t);
  }

  /**
   * Internal method to insert into a subtree.
   * x is the item to insert.
   * t is the node that roots the subtree.
   * i is the priority of x
   * Set the new root of the subtree.
   */
  void insert(ID &&x, AvlNode *&t, int i) {
    if (t == nullptr)
      t = new AvlNode{std::move(x), i, nullptr, nullptr};
    else if (x < t->element)
      insert(std::move(x), t->left, i);
    else if (t->element < x)
      insert(std::move(x), t->right, i);

    balance(t);
  }

  /**
   * Internal method to remove from a subtree.
   * x is the item to remove.
   * t is the node that roots the subtree.
   * Set the new root of the subtree.
   */
  void remove(const ID &x, AvlNode *&t) {
    if (t == nullptr)
      return; // Item not found; do nothing

    if (x < t->element)
      remove(x, t->left);
    else if (t->element < x)
      remove(x, t->right);
    else if (t->left != nullptr && t->right != nullptr) // Two children
    {
      t->element = findMin(t->right)->element;
      t->index = findMin(t->right)->index; // Move the index too!!!!
      remove(t->element, t->right);
    } else {
      AvlNode *oldNode = t;
      t = (t->left != nullptr) ? t->left : t->right;
      delete oldNode;
    }

    balance(t);
  }

  static const int ALLOWED_IMBALANCE = 1;

  // Assume t is balanced or within one of being balanced
  void balance(AvlNode *&t) {
    if (t == nullptr)
      return;

    if (height(t->left) - height(t->right) > ALLOWED_IMBALANCE) {
      if (height(t->left->left) >= height(t->left->right))
        {rotateWithLeftChild(t);}
      else
        {doubleWithLeftChild(t);}
      }
    else if (height(t->right) - height(t->left) > ALLOWED_IMBALANCE) {
      if (height(t->right->right) >= height(t->right->left))
        {rotateWithRightChild(t);}
      else
        {doubleWithRightChild(t);}
      }

    t->height = max(height(t->left), height(t->right)) + 1;
  }

  /**
   * Internal method to find the smallest item in a subtree t.
   * Return node containing the smallest item.
   */
  AvlNode *findMin(AvlNode *t) const {
    if (t == nullptr)
      return nullptr;
    if (t->left == nullptr)
      return t;
    return findMin(t->left);
  }

  /**
   * Internal method to find the largest item in a subtree t.
   * Return node containing the largest item.
   */
  AvlNode *findMax(AvlNode *t) const {
    if (t != nullptr)
      while (t->right != nullptr)
        t = t->right;
    return t;
  }

  AvlNode *findNode(const ID &x, AvlNode *t) const {
    if (t == nullptr)
      return t;
    else if (x < t->element)
      return findNode(x, t->left);
    else if (t->element < x)
      return findNode(x, t->right);
    else
      return t; // Match
  }


  void makeEmpty(AvlNode *&t) {
    if (t != nullptr) {
      makeEmpty(t->left);
      makeEmpty(t->right);
      delete t;
    }
    t = nullptr;
  }

  /**
   * Internal method to print a subtree rooted at t in sorted order.
   */
  void printTree(AvlNode *t) const {
    if (t != nullptr) {
      printTree(t->left);
      cout << "(" << t->element << " , " << t->index << ")" << endl;
      printTree(t->right);
    }
  }

  void displayLinks(AvlNode *t, int depth, ostream &out) const {
    const int SHIFT = 4;
    if (t != nullptr) {
      for (int i = 0; i < SHIFT * depth; i++) {
        out << " ";
      }
      out << t->element << "," << t->index << ") @:" << t; //<< " " << &t;

      out << "  L:" << (t->left) << " R:" << (t->right) << endl;

      displayLinks(t->left, depth + 1, out);
      displayLinks(t->right, depth + 1, out);
    }
  }

  void traverseTreeIndex (AvlNode *t) {
    if (t != nullptr) {
      traverseTreeIndex(t->left);
      array[t->index].second = t;
      traverseTreeIndex(t->right);
    }
  }

  /**
   * Return the height of node t or -1 if nullptr.
   */
  int height(AvlNode *t) const { return t == nullptr ? -1 : t->height; }

  int max(int lhs, int rhs) const { return lhs > rhs ? lhs : rhs; }

  /**
   * Rotate binary tree node with left child.
   * For AVL trees, this is a single rotation for case 1.
   * Update heights, then set new root.
   */
  void rotateWithLeftChild(AvlNode *&k2) {
    AvlNode *k1 = k2->left;
    k2->left = k1->right;
    k1->right = k2;
    k2->height = max(height(k2->left), height(k2->right)) + 1;
    k1->height = max(height(k1->left), k2->height) + 1;
    k2 = k1;
  }

  /**
   * Rotate binary tree node with right child.
   * For AVL trees, this is a single rotation for case 4.
   * Update heights, then set new root.
   */
  void rotateWithRightChild(AvlNode *&k1) {
    AvlNode *k2 = k1->right;
    k1->right = k2->left;
    k2->left = k1;
    k1->height = max(height(k1->left), height(k1->right)) + 1;
    k2->height = max(height(k2->right), k1->height) + 1;
    k1 = k2;
  }

  /**
   * Double rotate binary tree node: first left child.
   * with its right child; then node k3 with new left child.
   * For AVL trees, this is a double rotation for case 2.
   * Update heights, then set new root.
   */
  void doubleWithLeftChild(AvlNode *&k3) {
    rotateWithRightChild(k3->left);
    rotateWithLeftChild(k3);
  }

  /**
   * Double rotate binary tree node: first right child.
   * with its left child; then node k1 with new right child.
   * For AVL trees, this is a double rotation for case 3.
   * Update heights, then set new root.
   */
  void doubleWithRightChild(AvlNode *&k1) {
    rotateWithLeftChild(k1->right);
    rotateWithRightChild(k1);
  }

  // Heap


  /**
   * Establish heap order property from an arbitrary
   * arrangement of items. Runs in linear time.
   */

  /**
   * Internal method to percolate down in the heap.
   * hole is the index at which the percolate begins.
   */
  void buildHeap() {
    for (int i = currentSize / 2; i > 0; --i)
      percolateDown(i);
  }

  void percolateDown(int hole) {
    int child;
    pair<ID, AvlNode *> tmp = std::move(array[hole]);

    for (; hole * 2 <= currentSize; hole = child) {
      child = hole * 2;
      if (child != currentSize && array[child + 1].first < array[child].first)
        ++child;
      if (array[child].first < tmp.first)
        array[hole] = std::move(array[child]);
      else
        break;
    }
    array[hole] = std::move(tmp);
  }
};
#endif