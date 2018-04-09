// Authors: partner1, partner2
// Lab 16

package edu.umd.cmsc132A;

import tester.Tester;

public class Lab16 { /* Intentionally blank; leave blank */ }





//-----------------------------------------------------------------------------
// Main

class Main {

  static <X> Cons<X> makeCyclic(Cons<X> head) {
    Cons<X> tail = head.last().get(); // safe because `head' is a Cons cell
    tail.rest = head;
    return head;
  }

  IListof<Integer> oneTwoThree =
          new Cons<>(1, new Cons<>(2, new Cons<>(3, new Mt<>())));
  IListof<Integer> cyclicOneTwoThree =
          makeCyclic(new Cons<>(1, new Cons<>(2, new Cons<>(3, new Mt<>()))));

  void testLength(Tester t) {
    t.checkExpect(oneTwoThree.isCyclic(), false);
    t.checkExpect(oneTwoThree.length(), 3);
    t.checkExpect(oneTwoThree.contains(0), false);
    t.checkExpect(oneTwoThree.contains(1), true);
    t.checkExpect(oneTwoThree.foldr((x, y) -> x + y, 0), 6);

    t.checkExpect(cyclicOneTwoThree.isCyclic(), true);
    // t.checkExpect(cyclicOneTwoThree.length(), 3);
    // t.checkExpect(cyclicOneTwoThree.contains(0), false);
    // t.checkExpect(cyclicOneTwoThree.contains(1), true);
    // t.checkExpect(cyclicOneTwoThree.foldr((x, y) -> x + y, 0), 6);
  }

  void testEquality(Tester t) {
    Integer one = 1;
    Integer otherOne = 1;
    // t.checkExpect(one == otherOne, ???);

    Integer oneThousand = 1000;
    Integer otherOneThousand = 1000;
    // t.checkExpect(oneThousand == otherOneThousand, ???);
  }
}
