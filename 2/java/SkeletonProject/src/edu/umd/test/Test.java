package edu.umd.test;

import tester.Tester;

public class Test {
    void testPassing(Tester t) {
        t.checkExpect(true, true);
    }

    void testFailing(Tester t) {
        t.checkExpect(false, true);
    }
}