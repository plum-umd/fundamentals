package edu.umd.cmsc132A;

import tester.Tester;

import javax.swing.*;
import java.util.Iterator;

public class Lab21 { /* Intentionally blank */ }

class Main {

  void test(Tester t) {

    String s1 = "Willful Suspension of Disbelief";
    String s2 = "I Came as a Rat";
    String s3 = "This Devil's Workday";
    String s4 = "Tiny Cities Made of Ashes";
    Listof<String> playlist =
            new Cons<>(s1,
                    new Cons<>(s2,
                            new Cons<>(s3,
                                    new Cons<>(s4,
                                            new Empty<>()))));

    FocusZipper<String> zip = new FocusZipper<>(playlist);

    t.checkExpect(zip.focus, s1);

  }

}