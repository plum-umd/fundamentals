package edu.umd.cmsc132A;

import tester.Tester;

class TestTable {

    void testTable(Tester t, Table<String,Number> tbl) {
        tbl.put("DVH", 100);

        t.checkExpect(tbl.containsKey("DVH"), true);
        t.checkExpect(tbl.containsKey("DP"), false);
        t.checkExpect(tbl.lookup("DVH").get(), 100);
        t.checkExpect(tbl.lookup("DP").isEmpty(), true);
        t.checkExpect(tbl.keys(), new Cons<>("DVH", new Empty<>()));
        t.checkExpect(tbl.vals(), new Cons<>(100, new Empty<>()));
        t.checkExpect(tbl.keyvals(), new Cons<>(new Pair<>("DVH", 100), new Empty<>()));

        tbl.put("DVH", 50);

        t.checkExpect(tbl.containsKey("DVH"), true);
        t.checkExpect(tbl.containsKey("DP"), false);
        t.checkExpect(tbl.lookup("DVH").get(), 50);
        t.checkExpect(tbl.lookup("DP").isEmpty(), true);
        t.checkExpect(tbl.keys(), new Cons<>("DVH", new Empty<>()));
        t.checkExpect(tbl.vals(), new Cons<>(50, new Empty<>()));
        t.checkExpect(tbl.keyvals(), new Cons<>(new Pair<>("DVH", 50), new Empty<>()));

        tbl.put("DP", 200);

        t.checkExpect(tbl.containsKey("DVH"), true);
        t.checkExpect(tbl.containsKey("DP"), true);
        t.checkExpect(tbl.lookup("DVH").get(), 50);
        t.checkExpect(tbl.lookup("DP").get(), 200);

        t.checkOneOf(tbl.keys(),
                new Cons<>("DVH", new Cons<>("DP", new Empty<>())),
                new Cons<>("DP", new Cons<>("DVH", new Empty<>())));
        t.checkOneOf(tbl.vals(),
                new Cons<>(50, new Cons<>(200, new Empty<>())),
                new Cons<>(200, new Cons<>(50, new Empty<>())));
        t.checkOneOf(tbl.keyvals(),
                new Cons<>(new Pair<>("DVH", 50),
                        new Cons<>(new Pair<>("DP", 200), new Empty<>())),
                new Cons<>(new Pair<>("DP", 200),
                        new Cons<>(new Pair<>("DVH", 50), new Empty<>())));
    }

    void testListTable(Tester t) {
        testTable(t, new ListTable<>());
    }

    void testHashTable(Tester t) {
        testTable(t, new HashTable<>());


        HashTable<Name, Integer> grades = new HashTable<>();

        grades.put(new Name("D", "VH"), 100);
        grades.put(new Name("D", "P"), 200);
        grades.put(new Name("W", "D"), 50);

        t.checkExpect(grades.lookup(new Name("D", "P")).get(), 200);

    }

}
