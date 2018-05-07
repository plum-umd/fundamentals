package edu.umd.cmsc132A;

import tester.Tester;

import java.util.Iterator;

public class Test { /* Intentionally blank */
}

class Main {

    Tree<String> tr = new Node<>("fred", new Node<>("wilma", new Leaf<>(), new Leaf<>()),
            new Node<>("barney", new Leaf<>(), new Leaf<>()));

    TreeVisitor<String, TreeTraversal<String>> nlr = new MakeNLR<>();

    void testIterator(Tester t) {
        Iterator<String> it = tr.accept(nlr).iterator();

        t.checkExpect(it.hasNext(), true);
        t.checkExpect(it.next(), "fred");
        t.checkExpect(it.hasNext(), true);
        t.checkExpect(it.next(), "wilma");
        t.checkExpect(it.hasNext(), true);
        t.checkExpect(it.next(), "barney");
        t.checkExpect(it.hasNext(), false);

        // Add tests for other iterators
    }

    // Tests for NLRNext visitor, the workhorse of NLR traversal
    void testNLRNext(Tester t) {
        NLRNext<String> next = new NLRNext<String>(new Empty<>());

        Leaf<String> l = new Leaf<>();
        Node<String> n1 = new Node<>("n1", l, l);
        Node<String> n2 = new Node<>("n2", n1, l);
        Node<String> n3 = new Node<>("n3", l, n1);
        Node<String> n4 = new Node<>("n4", n2, n3);

        t.checkExpect(n1.accept(next), new LeafTraversal<>());
        t.checkExpect(n2.accept(next), new NLR<>(n1));
        t.checkExpect(n3.accept(next), new NLR<>(n1));
        t.checkExpect(n4.accept(next), new NLR<>(n2, new Cons<>(n3, new Empty<>())));
    }

    // Tests for NLR
    void testNLR(Tester t) {
        Leaf<String> l = new Leaf<>();
        Node<String> n = new Node<>("a",
                new Node<>("b", l, l),
                new Node<>("c", l, l));

        NLR<String> nNlr = new NLR<>(n);

        t.checkExpect(nNlr.has(), true);
        t.checkExpect(nNlr.get(), "a");
        t.checkExpect(nNlr.next().has(), true);
        t.checkExpect(nNlr.next().get(), "b");
        t.checkExpect(nNlr.next().next().has(), true);
        t.checkExpect(nNlr.next().next().get(), "c");
        t.checkExpect(nNlr.next().next().next().has(), false);
    }
}