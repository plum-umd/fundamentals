package edu.umd.cmsc132A;

import tester.*;

import java.util.function.Function;

public class Lab14 {}


interface Counter {
  Integer getCount();
  Counter next();
  Counter reset();
}


class SillyCounter implements Counter {
  Integer count;
  Function<String, Counter> messageToCounter;

  SillyCounter() { this(0); }
  SillyCounter(Integer count) {
    this.count = count;
    this.messageToCounter =
            (cmd) -> {
              if ("next".equals(cmd)) {
                return new SillyCounter(count+1);
              } else {
                return new SillyCounter();
              }
            };
  }

  public Integer getCount() {
    return count;
  }

  public Counter next() {
    return this.messageToCounter.apply("next");
  }

  public Counter reset() {
    return this.messageToCounter.apply("reset");
  }
}


// Ex 1:
// class FunCounter implements Counter ...


// Ex 7:
// class ImpCounter implements Counter ...

class Main {

  boolean testSilly(Tester t) {
    Counter silly0 = new SillyCounter();
    Integer count0 = silly0.getCount();
    Counter silly1 = silly0.next();
    Integer count1 = silly1.getCount();
    Counter silly1b = silly0.next();
    Integer count1b = silly1b.getCount();
    Counter silly0b = silly1b.reset();
    Integer count0b = silly0b.getCount();
    return t.checkExpect(count0, silly0.getCount())
            && t.checkExpect(count1, silly1.getCount())
            && t.checkExpect(count1, count1b)
            && t.checkExpect(count1b, silly1b.getCount())
            && t.checkExpect(count0b, silly0b.getCount());
  }

  /* Ex 2-6:
   * Do not run until you've answered the lab questions.
  boolean testFun(Tester t) {
    Counter fun0 = new FunCounter();
    Integer count0 = fun0.getCount();
    Counter fun1 = fun0.next();
    Integer count1 = fun1.getCount();
    Counter fun1b = fun0.next();
    Integer count1b = fun1b.getCount();
    Counter fun0b = fun1b.reset();
    Integer count0b = fun0b.getCount();
    return t.checkExpect(count0, fun0.getCount())
            && t.checkExpect(count1, fun1.getCount())
            && t.checkExpect(count1, count1b)
            && t.checkExpect(count1b, fun1b.getCount())
            && t.checkExpect(count0b, fun0b.getCount());
  }*/


  /* Ex 8-12:
   * Do not run until you've answered the lab questions.
  boolean testImp(Tester t) {
    Counter imp0 = new ImpCounter();
    Integer count0 = imp0.getCount();
    Counter imp1 = imp0.next();
    Integer count1 = imp1.getCount();
    Counter imp1b = imp0.next();
    Integer count1b = imp1b.getCount();
    Counter imp0b = imp1b.reset();
    Integer count0b = imp0b.getCount();
    return t.checkExpect(count0, imp0.getCount())
            && t.checkExpect(count1, imp1.getCount())
            && t.checkExpect(count1, count1b)
            && t.checkExpect(count1b, imp1b.getCount())
            && t.checkExpect(count0b, imp0b.getCount());
  }*/


}
