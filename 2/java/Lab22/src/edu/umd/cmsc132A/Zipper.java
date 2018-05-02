package edu.umd.cmsc132A;

import java.util.Random;
import java.util.function.Function;

class FocusZipper<X> {
    static Random r = new Random();

    private Listof<X> context;
    X focus;
    private Listof<X> rest;

    public FocusZipper(Listof<X> elems) {
        this.focus = elems.accept(new ListVisitor<X, X>() {
            public X visitEmpty(Empty<X> mt) {
                throw new RuntimeException("Must be one or more elements!");
            }
            public X visitCons(Cons<X> cons) {
                FocusZipper.this.rest = cons.rest;
                return cons.first;
            }
        });
        this.context = AListof.empty();
    }

    private FocusZipper(Listof<X> context, X focus, Listof<X> rest) {
        this.context = context;
        this.focus = focus;
        this.rest = rest;
    }

    // Create a new zipper focused one element to the left;
    // If there are no more elements return this zipper.
    public FocusZipper<X> left() {
        return this.context.accept(new ListVisitor<X, FocusZipper<X>>() {
            public FocusZipper<X> visitEmpty(Empty<X> mt) {
                return FocusZipper.this;
            }
            public FocusZipper<X> visitCons(Cons<X> cons) {
                return new FocusZipper<>(cons.rest, cons.first, rest.cons(focus));
            }
        });
    }

    // Create a new zipper focused one element to the right;
    // If there are no more elements return this zipper.
    public FocusZipper<X> right() {
        return this.rest.accept(new ListVisitor<X, FocusZipper<X>>() {
            public FocusZipper<X> visitEmpty(Empty<X> mt) {
                return FocusZipper.this;
            }
            public FocusZipper<X> visitCons(Cons<X> cons) {
                return new FocusZipper<>(context.cons(focus), cons.first, cons.rest);
            }
        });
    }

    // Create a new zipper focused at the start
    public FocusZipper<X> start() {
        return this.context.accept(new ListVisitor<X, FocusZipper<X>>() {
            public FocusZipper<X> visitEmpty(Empty<X> mt) {
                return FocusZipper.this;
            }
            public FocusZipper<X> visitCons(Cons<X> cons) {
                return left().start();
            }
        });
    }

    // Create a new zipper focused at the last element
    public FocusZipper<X> end() {
        return this.rest.accept(new ListVisitor<X, FocusZipper<X>>() {
            public FocusZipper<X> visitEmpty(Empty<X> mt) {
                return FocusZipper.this;
            }
            public FocusZipper<X> visitCons(Cons<X> cons) {
                return right().end();
            }
        });
    }

    // Create a new zipper focused one element to the left;
    // If there are no more elements, cycle around to the
    // end of the zipper.
    public FocusZipper<X> leftCycle() {
        return this.context.accept(new ListVisitor<X, FocusZipper<X>>() {
            public FocusZipper<X> visitEmpty(Empty<X> mt) {
                return end();
            }
            public FocusZipper<X> visitCons(Cons<X> cons) {
                return left();
            }
        });
    }

    // Create a new zipper focused one element to the right;
    // If there are no more elements, cycle around to the
    // front of the zipper.
    public FocusZipper<X> rightCycle() {
        return this.rest.accept(new ListVisitor<X, FocusZipper<X>>() {
            public FocusZipper<X> visitEmpty(Empty<X> mt) {
                return start();
            }
            public FocusZipper<X> visitCons(Cons<X> cons) {
                return right();
            }
        });
    }

    // Count the elements of this zipper
    public Integer count() {
        return this.context.foldr((x, n) -> n+1,
                this.rest.foldr((y, m) -> m+1, 1));
    }

    // Focus on the nth element to the right from this zipper
    public FocusZipper<X> nth(Integer n) {
        if (n > 0) {
            return this.rightCycle().nth(n-1);
        } else if (n < 0) {
            return this.leftCycle().nth(n+1);
        } else { // n.equals(0)
            return this;
        }
    }

    // Focus on a randomly chosen element of this zipper
    public FocusZipper<X> random() {
        return nth(r.nextInt(count()));
    }

    // Return the underlying list of this zipper
    public Listof<X> unzip() {
        Listof<X> tmp = this.rest.cons(this.focus);
        for (X x : this.context) {
            tmp = tmp.cons(x);
        }
        return tmp;
    }

    // Apply the function `f' to every element in this zipper
    public <R> FocusZipper<R> map(Function<X, R> f) {
        return new FocusZipper<>(
                this.context.map(f),
                f.apply(this.focus),
                this.rest.map(f));
    }

    // Ex 3:
    // Apply the function `f' to the focus of this zipper
    public FocusZipper<X> updateFocus(Function<X, X> f) {
        return this;
    }

    // Creates a string representation of this zipper
    public String toString() {
        return "⟨" + context + ", " + focus + ", " + rest + "⟩";
    }

}