package edu.umd.cmsc132A;

import tester.Tester;

public class Lab19 { /* Intentionally blank; leave blank */ }

//-----------------------------------------------------------------------------
// Main

class Main {

  IStackof<Integer> stack;

  void initData() {
    this.stack = new Stackof<>();
    this.stack.push(1);
    this.stack.push(2);
    this.stack.push(3);
  }

  void testPushPop(Tester t) {
    this.initData();
    Integer n = this.stack.height();
    this.stack.push(0);
    t.checkExpect(this.stack.height(), n+1);
    t.checkExpect(this.stack.pop().get(), 0);
    t.checkExpect(this.stack.height(), n);
    this.stack.push(0);
    this.stack.push(42);
    t.checkExpect(this.stack.height(), n+2);
    t.checkExpect(this.stack.pop().get(), 42);
    this.stack.pop();
    this.stack.pop();
    this.stack.pop();
    this.stack.pop();
    t.checkExpect(this.stack.pop().isPresent(), false);
  }

  IStackof<Integer> oStack;

  void initOrdData() {
    this.oStack = new OrdStackof<>();
    this.oStack.push(3);
    this.oStack.push(2);
    this.oStack.push(1);
  }

  void testOrdPushPop(Tester t) {
    this.initOrdData();
    Integer n = this.oStack.height();
    this.oStack.push(0);
    t.checkExpect(this.oStack.height(), n+1);
    t.checkExpect(this.oStack.pop().get(), 0);
    t.checkExpect(this.oStack.height(), n);
    this.oStack.push(4);
    t.checkExpect(this.oStack.height(), n); // 4 was not pushed on
    this.oStack.push(-4);
    t.checkExpect(this.oStack.height(), n+1); // -4 was pushed on
    t.checkExpect(this.oStack.pop().get(), -4);
    this.oStack.pop();
    this.oStack.pop();
    this.oStack.pop();
    t.checkExpect(this.oStack.pop().isPresent(), false);
  }

}
