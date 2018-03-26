package edu.umd.cmsc132A;

import tester.*;

public class Lab13 {}

// List and ListVisitor interfaces

interface ListVisitor<T, U> {
  U visitMt(Mt<T> empty);
  U visitCons(Cons<T> cons);
}

interface List<T> {
  <U> U accept(ListVisitor<T, U> visitor);
}

// Mt and Cons List classes

class Mt<T> implements List<T> {

  public <U> U accept(ListVisitor<T, U> visitor) {
    return visitor.visitMt(this);
  }

}

class Cons<T> implements List<T> {
  T first;
  List<T> rest;

  Cons(T first, List<T> rest) {
    this.first = first;
    this.rest  = rest;
  }

  public <U> U accept(ListVisitor<T, U> visitor) {
    return visitor.visitCons(this);
  }
}

// List visiting operations

// Sum the values of any List<Integer>
class ListSum implements ListVisitor<Integer, Integer> {
  public Integer visitCons(Cons<Integer> cons) {
    return cons.first + cons.rest.accept(this);
  }
  public Integer visitMt(Mt<Integer> empty) {
    return 0;
  }
}

// Ex 1:
// Count the number of elements of any List<Integer>
// class ListLength<T> implements ...

// Ex 2:
// Append some suffix to any list
// class ListAppend<T> implements ...

// Ex 3:
// Return the Nth element of any list, if it exists
// class ListNth<T> implements ...

// Ex 4:
// Return the reverse of any list
// class ListReverse<T> implements ...


// BT and BTVisitor interfaces

// Ex 5:
// interface BTVisitor<...

interface BT<T> {
  // Ex 6:
}

// Leaf and Node BT classes

class Leaf<T> implements BT<T> {

  // Ex 6:

}

class Node<T> implements BT<T> {
  T value;
  BT<T> left;
  BT<T> right;

  Node(T value, BT<T> left, BT<T> right) {
    this.value = value;
    this.left = left;
    this.right = right;
  }

  // Ex 6:

}

// BT visiting operations

// Ex 7:
// Calculate the sum of Integer BTs
// class BTSum<T> implements ...

// Ex 8:
// Calculate the depth of BTs
// class BTDepth<T> implements ...

// Ex 9:
// Mirror the given BT
// class BTMirror<T> implements ...

// Ex 10:
// Return the in-order traversal of the BT
// class BTInOrder<T> implements ...

class Main {

  // '(4 3 2 1)
  List<Integer> l0 = new Mt<>();
  List<Integer> l1 = new Cons<>(1, l0);
  List<Integer> l2 = new Cons<>(2, l1);
  List<Integer> l3 = new Cons<>(3, l2);
  List<Integer> l4 = new Cons<>(4, l3);

  boolean testListSum(Tester t) {
    ListSum sum = new ListSum();
    return t.checkExpect(l0.accept(sum), 0)
            && t.checkExpect(l1.accept(sum), 1)
            && t.checkExpect(l2.accept(sum), 3)
            && t.checkExpect(l3.accept(sum), 6)
            && t.checkExpect(l4.accept(sum), 10);
  }

  boolean testListLength(Tester t) {
    return false;
  }

  boolean testListNth(Tester t) {
    return false;
  }

  boolean testListAppend(Tester t) {
    return false;
  }

  boolean testListReverse(Tester t) {
    return false;
  }

  //    bt4: 4
  //        / \
  //  bt2: 2
  //      / \
  //     1   3
  //    / \ / \
  BT<Integer> bt0 = new Leaf<>();
  BT<Integer> bt1 = new Node(1, bt0, bt0);
  BT<Integer> bt3 = new Node(3, bt0, bt0);
  BT<Integer> bt2 = new Node(2, bt1, bt3);
  BT<Integer> bt4 = new Node(4, bt2, bt0);

  boolean testBTDepth(Tester t) {
    return false;
  }

  boolean testBTSum(Tester t) {
    return false;
  }

  boolean testBTMirror(Tester t) {
    return false;
  }

  boolean testBTInOrder(Tester t) {
    return false;
  }

}
