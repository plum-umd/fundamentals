// Authors: partner1, partner2
// Lab 9

package edu.umd.cmsc132A;

import tester.Tester;
import javalib.worldimages.*;
import javalib.funworld.*;
import java.awt.Color;
import java.util.function.*;
import java.util.Random;
import java.util.function.IntUnaryOperator;

public class Lab10 { /* Intentionally blank; leave blank */ }


// Exercise 1
// interface Shape

// Exercise 2
// class Circle implements Shape

// Exercise 3
// class Rectangle implements Shape

// Exercise 4
// class Square implements Shape

// Exercise 5
// interface Volume

// Exercise 6
// class Liter implements Volume

// Exercise 7
// class Cup implements Volume

// Exercise 8
// class Gallon implements Volume

// Exercise 9
// Your own class here.

// Exercise 10
// Modify the above classes.

// Exercise 11

interface Directory {
  Boolean exists(Predicate<String> p);
}

interface ListOfDir {
  Boolean exists(Predicate<String> p);
}

class MtLoD implements ListOfDir {
  MtLoD(){}

  public Boolean exists(Predicate<String> p) {
    return false;
  }
}

class ConsLoD implements ListOfDir {
  Directory first;
  ListOfDir rest;
  ConsLoD(Directory first, ListOfDir rest) {
    this.first = first;
    this.rest = rest;
  }

  public Boolean exists(Predicate<String> p) {
    return false;
  }
}

class Dir implements Directory {
  String name;
  ListOfDir dirs;

  Dir(String name) {
    this(name, new MtLoD());
  }

  Dir(String name, ListOfDir dirs) {
    this.name = name;
    this.dirs = dirs;
  }

  public Boolean exists(Predicate<String> p) {
    return false;
  }
}


//-----------------------------------------------------------------------------
// Main

// Write your tests here
class Main {
  Boolean testTrivial(Tester t) {
    return t.checkExpect(1, 1);
  }
}
