package edu.umd.cmsc132A;

import tester.Tester;

import java.util.Iterator;

public class Test { /* Intentionally blank */
}

class Main {
    void testMain(Tester t) {

        Tree<Integer> l = new Leaf<>();
        Tree<Integer> tr = new Node<>(1,
                new Node<>(2, new Node<>(3, l, l), new Node<>(4, l, l)),
                new Node<>(5, l, new Node<>(6, l, l)));

        Iterator<Integer> i = tr.iteratorPreOrder();
        t.checkExpect(i.hasNext(), true);
        t.checkExpect(i.next(), 1);
        t.checkExpect(i.hasNext(), true);
        t.checkExpect(i.next(), 2);
        t.checkExpect(i.hasNext(), true);
        t.checkExpect(i.next(), 3);
        t.checkExpect(i.hasNext(), true);
        t.checkExpect(i.next(), 4);
        t.checkExpect(i.hasNext(), true);
        t.checkExpect(i.next(), 5);
        t.checkExpect(i.hasNext(), true);
        t.checkExpect(i.next(), 6);
        t.checkExpect(i.hasNext(), false);
        t.checkException(new RuntimeException("No next element"), i, "next");

        Integer sum = 0;
        for (Integer j : tr) {
            sum = sum + j;
        }
        t.checkExpect(sum, 21);
    }
}