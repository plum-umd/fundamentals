package edu.umd.cmsc132A;

import javax.swing.text.html.Option;
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

  interface Listof<X> {
    <Y> Y foldr(BiFunction<X, Y, Y> f, Y b);
    Optional<X> first();
    Optional<Listof<X>> rest();
  }

  class Empty<X> implements Listof<X> {
    public <Y> Y foldr(BiFunction<X, Y, Y> f, Y b) { return b; }
    public Optional<X> first() { return Optional.empty(); }
    public Optional<Listof<X>> rest() { return Optional.empty(); }
  }

  class Cons<X> implements Listof<X> {
    X first;
    Listof<X> rest;

    Cons(X first, Listof<X> rest) {
      this.first = first;
      this.rest = rest;
    }

    public <Y> Y foldr(BiFunction<X, Y, Y> f, Y b) {
      return f.apply(this.first, this.rest.foldr(f, b));
    }

    public Optional<X> first() { return Optional.of(this.first); }
    public Optional<Listof<X>> rest() { return Optional.of(this.rest); }
  }
  
  private Listof<X> stack;

  Stackof() {
    this.stack = new Empty();
  }

  // Ex 1:mak
  // EFFECT:
  // Pushes the element `x' on to the top of this stack
  public void push(X x) {
    return;
  }

  // Ex 1:
  // EFFECT:
  // Removes and returns the top of this stack, if it exists
  public Optional<X> pop() {
    return Optional.empty();
  }

  // Ex 2:
  // The fundamental stack abstraction
  public <Y> Y foldr(BiFunction<X, Y, Y> f, Y b) {
    return b;
  }

  // Ex 3:
  // Return the height of this stack
  public Integer height() {
    return 0;
  }

}

class OrdStackof<X extends Comparable<X>> extends Stackof<X> {

  // Ex 4:
  public void push(X x) {
    return;
  }
  
}
