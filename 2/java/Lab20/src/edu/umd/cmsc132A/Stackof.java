package edu.umd.cmsc132A;

import java.util.function.BiFunction;
import java.util.Optional;

interface IStackof<X> {

  // EFFECT:
  // Push the element `x' on to the top of this stack
  void push(X x);

  // EFFECT:
  // Remove and return the top of this stack, if it exists
  Optional<X> pop();

  // Fundamental stack abstraction
  <Y> Y foldr(BiFunction<X, Y, Y> f, Y b);

  Integer height();
}

class Stackof<X> implements IStackof<X> {
  
  private Listof<X> stack;

  Stackof() {
    this.stack = new Empty();
  }

  // EFFECT:
  // Pushes the element `x' on to the top of this stack
  public void push(X x) {
    this.stack = new Cons(x, this.stack);
  }

  // EFFECT:
  // Removes and returns the top of this stack, if it exists
  public Optional<X> pop() {
    Optional<X> top = this.stack.first();
    if (top.isPresent()) {
      this.stack = this.stack.rest().get();
    }
    return top;
  }

  // The fundamental stack abstraction
  public <Y> Y foldr(BiFunction<X, Y, Y> f, Y b) {
    return this.stack.foldr(f, b);
  }

  // Return the height of this stack
  public Integer height() {
    return this.foldr((x, n) -> n+1, 0);
  }

}

class OrdStackof<X extends Comparable<X>> extends Stackof<X> {

  // Ex 1:
  public Boolean canPush(X x) {
    return false;
  }

  // Ex 2 (use `canPush' to remove duplicate code):
  public void push(X x) {
    Optional<X> top = super.pop();
    if (top.isPresent()) {
      X actualTop = top.get();
      super.push(actualTop);
      if (x.compareTo(actualTop) < 0) {
        super.push(x);
      }
    } else {
      super.push(x);
    }
  }
  
}
