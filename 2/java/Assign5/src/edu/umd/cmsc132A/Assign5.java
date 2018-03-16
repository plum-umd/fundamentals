// Authors: partner1, partner2
// Assignment 5

package edu.umd.cmsc132A;

import tester.Tester;

public class Assign5 {

}

// Write test cases here, run "Test" configuration to test
class Tests {

    // A few sets to help write tests
    Set<Integer> mti = ASet.empty();
    Set<Integer> s1 = mti.add(1).add(2).add(3);
    Set<Integer> s2 = mti.add(3).add(2).add(1);

    Boolean testSame(Tester t) {
        return t.checkExpect(s1.same(s2), true) &&
                t.checkExpect(s2.same(s1), true) &&
                t.checkExpect(s1.add(1).same(s2), true) &&
                t.checkExpect(s1.add(4).same(s2), false) &&
                t.checkExpect(s2.same(s1.add(4)), false);
    }

    Boolean testContains(Tester t) {
        return t.checkExpect(s1.contains(1), true) &&
                t.checkExpect(s1.contains(2), true) &&
                t.checkExpect(s1.contains(3), true) &&
                t.checkExpect(s1.contains(4), false);
    }

    Boolean testMap(Tester t) {
        Set<Integer> s3 = mti.add(2).add(3).add(4);
        return t.checkExpect(mti.map(x -> x + 1).same(mti), true) &&
                t.checkExpect(s1.map(x -> x + 1).same(s3), true) &&
                t.checkExpect(s1.map(x -> 0).same(mti.add(0)), true);
    }

    Boolean testElem(Tester t) {
        return t.checkExpect(mti.elem().isPresent(), false) &&
                t.checkExpect(s1.elem().isPresent(), true) &&
                t.checkExpect(s1.elem().filter(i -> 0 <= i && i <= 3)
                        .isPresent(), true) &&
                t.checkExpect(mti.add(1).elem().get(), 1);
    }

    Boolean testExists(Tester t) {
        return t.checkExpect(mti.exists(x -> true), false) &&
                t.checkExpect(s1.exists(x -> x.equals(2)), true) &&
                t.checkExpect(s1.exists(x -> x > 3), false);
    }

    // You do not need to add more tests for lists unless you add list methods
    Boolean testLists(Tester t) {
        Listof<Integer> mti = AListof.empty();
        Listof<Integer> i1 = mti.cons(1).cons(2).cons(3); // [3,2,1]
        Listof<Integer> i2 = mti.cons(2).cons(3).cons(4); // [4,3,2]
        Listof<Pairof<Integer,Integer>> mtp = AListof.empty();
        Listof<Pairof<Integer,Integer>> i1i2 = mtp
                        .cons(new Pairof<>(1,2))
                        .cons(new Pairof<>(2,3))
                        .cons(new Pairof<>(3,4));

        return t.checkExpect(i1.length(), 3) &&
                t.checkExpect(i1.map(i -> i+1), i2) &&
                t.checkExpect(i1.foldr((i, s) -> s+i, 3), 9) &&
                t.checkExpect(mti.append(i1), i1) &&
                t.checkExpect(i1.append(mti), i1) &&
                t.checkExpect(i1.same(i1), true) &&
                t.checkExpect(i1.same(i2), false) &&
                t.checkExpect(i2.same(i1), false) &&
                t.checkExpect(i1.zip(mti), mtp) &&
                t.checkExpect(mti.zip(i1), mtp) &&
                t.checkExpect(i1.zip(i2), i1i2);
    }
}
