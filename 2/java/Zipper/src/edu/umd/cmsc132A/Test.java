package edu.umd.cmsc132A;

import tester.Tester;

import javax.swing.*;
import java.util.Iterator;

public class Test { /* Intentionally blank */ }

class Main {

  void test(Tester t) {

    Iterator<Integer> l123 = AListof.make(3, n -> n+1).iterator();
    Iterator<Integer> l456 = AListof.make(3, n -> n+4).iterator();
    System.out.println(l123);
    System.out.println(l456);

    Iterable<Integer> alt = new Alt<>(l123, l456);

    for (Integer i : alt) {
      System.out.println(i);
      // Prints 1 4 2 5 3 6
    }

    // System.out.println(l123.next());
    // no more next!!!

    ListZipper<Integer> zip = new ListZipper(AListof.make(3, n -> n+1));
    System.out.println(zip);
    System.out.println(zip.right());
    System.out.println(zip.right().right());
    System.out.println(zip.right().right().right());

    System.out.println("Zip");
    for (Integer i : zip) {
      System.out.println(zip);
      System.out.println(i);
      // Print 1 2 3
    }

    System.out.println("Left");
    for (Integer i : zip.left().left().left()) {
      System.out.println(i);
      // Print 1 2 3
    }

    System.out.println("Start");
    for (Integer i : zip.start()) {
      System.out.println(i);
      // Print 1 2 3
    }

    // No more elems
    System.out.println(zip);

    System.out.println("Flip");
    for (Integer i : zip.flip()) {
      System.out.println(i);
      // Print 3 2 1
    }

  }

}