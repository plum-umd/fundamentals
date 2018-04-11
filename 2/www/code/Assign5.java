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

    MultiSet<Integer> ms1 = mti.toMultiSet().add(1).add(2).add(3);
    MultiSet<Integer> ms2 = mti.toMultiSet().add(3).add(2).add(1);

    Map<Integer, String> mtm = AMap.empty();
    Map<Integer, String> m1 = mtm.add(1, "one").add(2, "two").add(3, "three");
    Map<Integer, String> m2 = mtm.add(3, "three").add(2, "two").add(1, "one");

    void testPublicMap(Tester t) {

        // add
        t.checkExpect(mtm.add(0, "zero").count().equals(1), true);
        t.checkExpect(m1.add(4, "four").count().equals(4), true);
        t.checkExpect(m1.add(1, "uno").count().equals(3), true);

        // count
        t.checkExpect(mtm.count().equals(0), true);
        t.checkExpect(m1.count().equals(3), true);

        // lookup
        t.checkExpect(mtm.lookup(0).isPresent(), false);
        t.checkExpect(m1.lookup(1).isPresent(), true);
        t.checkExpect(m1.lookup(1).get().equals("one"), true);
        t.checkExpect(m1.lookup(3).isPresent(), true);
        t.checkExpect(m1.lookup(3).get().equals("three"), true);
        t.checkExpect(m1.add(1, "uno").lookup(1).isPresent(), true);
        t.checkExpect(m1.add(1, "uno").lookup(1).get().equals("uno"), true);

        Map<Integer, String> sing = mtm.add(1, "eins");

        // keyVals
        t.checkExpect(mtm.keyVals().count().equals(0), true);
        t.checkExpect(sing.keyVals().count().equals(1), true);
        t.checkExpect(m1.keyVals().count().equals(3), true);
        t.checkExpect(sing.keyVals().elem().get().left.equals(1), true);
        t.checkExpect(sing.keyVals().elem().get().right.equals("eins"), true);

        // keys
        t.checkExpect(mtm.keys().count().equals(0), true);
        t.checkExpect(sing.keys().count().equals(1), true);
        t.checkExpect(m1.keys().count().equals(3), true);
        t.checkExpect(sing.keys().elem().get().equals(1), true);

        // vals
        t.checkExpect(mtm.vals().length().equals(0), true);
        t.checkExpect(sing.vals().length().equals(1), true);
        t.checkExpect(m1.vals().length().equals(3), true);

        // same
        t.checkExpect(mtm.same(mtm), true);
        t.checkExpect(m1.same(m1), true);
        t.checkExpect(m1.same(mtm), false);
        t.checkExpect(mtm.same(m1), false);
        t.checkExpect(m1.same(m2), true);
        t.checkExpect(m2.same(m1), true);
        t.checkExpect(m1.add(1, "uno").same(m1), false);
        t.checkExpect(m1.same(m1.add(1, "uno")), false);
        t.checkExpect(m1.same(m1.add(1, "one")), true);
        t.checkExpect(m1.same(m1.add(2, "two")), true);

        // remove
        t.checkExpect(mtm.remove(1).count().equals(0), true);
        t.checkExpect(sing.remove(1).count().equals(0), true);
        t.checkExpect(m1.remove(4).count().equals(3), true);
        t.checkExpect(m1.remove(3).count().equals(2), true);
        t.checkExpect(m1.remove(2).count().equals(2), true);

        // removeAll
        t.checkExpect(mtm.removeAll(k -> false).count().equals(0), true);
        t.checkExpect(mtm.removeAll(k -> true).count().equals(0), true);
        t.checkExpect(m1.removeAll(k -> true).count().equals(0), true);
        t.checkExpect(m1.removeAll(k -> k.equals(2)).count().equals(2), true);

        // removeAllPairs
        t.checkExpect(mtm.removeAllPairs((k, v) -> false).count().equals(0), true);
        t.checkExpect(mtm.removeAllPairs((k, v) -> true).count().equals(0), true);
        t.checkExpect(m1.removeAllPairs((k, v) -> true).count().equals(0), true);
        t.checkExpect(m1.removeAllPairs((k, v) -> k.equals(2) && v.equals("two")).count().equals(2), true);
        t.checkExpect(m1.removeAllPairs((k, v) -> k.equals(2) && v.equals("dos")).count().equals(3), true);

        // join
        t.checkExpect(mtm.join(mtm).count().equals(0), true);
        t.checkExpect(mtm.join(m1).count().equals(3), true);
        t.checkExpect(m1.join(m1).count().equals(3), true);

        t.checkExpect(m1.join(sing).count().equals(3), true);
        t.checkExpect(sing.join(m1).count().equals(3), true);
        t.checkExpect(m1.join(sing).lookup(1).get().equals("one"), true);
        t.checkExpect(sing.join(m1).lookup(1).get().equals("eins"), true);
        t.checkExpect(m1.join(sing).lookup(2).get().equals("two"), true);
    }

    void testPublicMultiSet(Tester t) {
        // add
        t.checkExpect(ms1.add(4).count().equals(4), true);
        t.checkExpect(ms1.add(1).count().equals(4), true);

        // map
        t.checkExpect(ms1.map(i -> 0).count().equals(3), true);
        t.checkExpect(ms1.map(i -> i).count().equals(3), true);

        // count
        t.checkExpect(mti.toMultiSet().count().equals(0), true);
        t.checkExpect(ms1.count().equals(3), true);

        // same
        t.checkExpect(ms1.same(ms2), true);
        t.checkExpect(ms2.same(ms1), true);
        t.checkExpect(ms1.add(1).same(ms1), false);
        t.checkExpect(ms1.add(1).same(ms1.add(1)), true);

        // superset
        t.checkExpect(ms1.superset(mti.toMultiSet()), true);
        t.checkExpect(ms1.superset(ms1), true);
        t.checkExpect(ms1.add(1).superset(ms1), true);
        t.checkExpect(ms1.superset(ms1.add(1)), false);

        // subset
        t.checkExpect(ms1.subset(mti.toMultiSet()), false);
        t.checkExpect(ms1.subset(ms1), true);
        t.checkExpect(ms1.add(1).subset(ms1), false);
        t.checkExpect(ms1.subset(ms1.add(1)), true);

        // contains
        t.checkExpect(mti.toMultiSet().contains(0), false);
        t.checkExpect(ms1.contains(1), true);
        t.checkExpect(ms1.contains(3), true);
        t.checkExpect(ms1.contains(4), false);
        t.checkExpect(ms1.add(4).contains(4), true);

        // exists
        t.checkExpect(mti.toMultiSet().exists(i -> true), false);
        t.checkExpect(ms1.exists(i -> i.equals(2)), true);
        t.checkExpect(ms1.exists(i -> i > 3), false);

        // forAll
        t.checkExpect(mti.toMultiSet().forAll(i -> false), true);
        t.checkExpect(ms1.forAll(i -> true), true);
        t.checkExpect(ms1.forAll(i -> i > 0), true);
        t.checkExpect(ms1.forAll(i -> i.equals(2)), false);

        // toList
        t.checkExpect(mti.toMultiSet().toList().length().equals(0), true);
        t.checkExpect(ms1.add(1).toList().length().equals(4), true);

        // elem
        t.checkExpect(mti.toMultiSet().elem().isPresent(), false);
        t.checkExpect(ms1.elem().isPresent(), true);
        t.checkExpect(mti.toMultiSet().add(4).elem().get().equals(4), true);

        // choose
        t.checkExpect(mti.toMultiSet().choose().isPresent(), false);
        t.checkExpect(mti.toMultiSet().add(4).choose().get().left.equals(4), true);

        Pairof<Integer, MultiSet<Integer>> p = ms1.choose().get();
        t.checkExpect(p.right.contains(p.left), false);

        // toSet
        t.checkExpect(mti.toMultiSet().toSet().count().equals(0), true);
        t.checkExpect(ms1.toSet().count().equals(3), true);
        t.checkExpect(ms1.add(1).toSet().count().equals(3), true);
    }

    void testPublicSet(Tester t) {
        // count
        t.checkExpect(s1.count().equals(3), true);
        t.checkExpect(s1.add(1).count().equals(3), true);
        t.checkExpect(s1.add(4).count().equals(4), true);

        // map
        t.checkExpect(mti.map(x -> x).count().equals(0), true);
        t.checkExpect(s1.map(x -> x).count().equals(3), true);
        t.checkExpect(s1.map(x -> 0).count().equals(1), true);

        // same
        t.checkExpect(mti.same(mti), true);
        t.checkExpect(mti.same(s1), false);
        t.checkExpect(s1.same(mti), false);
        t.checkExpect(s1.same(s1), true);
        t.checkExpect(s1.same(s2), true);
        t.checkExpect(s2.same(s1), true);
        t.checkExpect(s1.add(1).same(s1), true);
        t.checkExpect(s1.add(4).same(s1), false);

        // superset
        t.checkExpect(mti.superset(mti), true);
        t.checkExpect(mti.superset(s1), false);
        t.checkExpect(s1.superset(mti), true);
        t.checkExpect(s1.superset(mti.add(4)), false);

        // subset
        t.checkExpect(mti.subset(mti), true);
        t.checkExpect(mti.subset(s1), true);
        t.checkExpect(s1.subset(mti), false);
        t.checkExpect(s1.subset(mti.add(4)), false);

        // contains
        t.checkExpect(mti.contains(0), false);
        t.checkExpect(s1.contains(1), true);
        t.checkExpect(s1.contains(2), true);
        t.checkExpect(s1.contains(3), true);
        t.checkExpect(s1.contains(4), false);
        t.checkExpect(s1.add(4).contains(4), true);

        // exists
        t.checkExpect(mti.exists(x -> true), false);
        t.checkExpect(s1.exists(x -> true), true);
        t.checkExpect(s1.exists(x -> false), false);
        t.checkExpect(s1.exists(x -> x.equals(2)), true);

        // forAll
        t.checkExpect(mti.forAll(x -> false), true);
        t.checkExpect(s1.forAll(x -> false), false);
        t.checkExpect(s1.forAll(x -> x.equals(2)), false);
        t.checkExpect(s1.forAll(x -> x > 0), true);

        // toList
        t.checkExpect(mti.toList().length().equals(0), true);
        t.checkExpect(s1.toList().length().equals(3), true);
        t.checkExpect(s1.add(4).toList().length().equals(4), true);
        t.checkExpect(s1.add(2).toList().length().equals(3), true);

        // elem
        t.checkExpect(mti.elem().isPresent(), false);
        t.checkExpect(s1.elem().isPresent(), true);
        t.checkExpect(mti.add(4).elem().orElse(0).equals(4), true);

        // choose
        t.checkExpect(mti.choose().isPresent(), false);
        t.checkExpect(s1.choose().isPresent(), true);
        t.checkExpect(mti.add(4).choose().get().left, 4);
        t.checkExpect(mti.add(4).choose().get().right.count(), 0);

        Pairof<Integer, Set<Integer>> s = s1.choose().get();
        t.checkExpect(s.right.contains(s.left), false);

        // toMultiSet
        t.checkExpect(mti.toMultiSet().count(), 0);
        t.checkExpect(s1.toMultiSet().count(), 3);
        t.checkExpect(s1.add(3).toMultiSet().count(), 3);
    }
}
