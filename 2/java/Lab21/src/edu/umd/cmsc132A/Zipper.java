package edu.umd.cmsc132A;

import java.util.Optional;
import java.util.Random;

class FocusZipper<X> {
    // Used in Ex 7
    static Random r = new Random();

    private Listof<X> context;
    X focus;
    private Listof<X> rest;

    public FocusZipper(Listof<X> elems) {
        // Ex 1
    }

    private FocusZipper(Listof<X> context, X focus, Listof<X> rest) {
        this.context = context;
        this.focus = focus;
        this.rest = rest;
    }

    // Ex 2:
    // Create a new zipper focused one element to the left;
    // If there are no more elements return this zipper.
    public FocusZipper<X> left() {
        return this;
    }

    // Ex 2:
    // Create a new zipper focused one element to the right;
    // If there are no more elements return this zipper.
    public FocusZipper<X> right() {
        return this;
    }

    // Ex 3:
    // Create a new zipper focused at the start
    public FocusZipper<X> start() {
        return this;
    }

    // Ex 3:
    // Create a new zipper focused at the last element
    public FocusZipper<X> end() {
        return this;
    }

    // Ex 4:
    // Create a new zipper focused one element to the left;
    // If there are no more elements, cycle around to the
    // end of the zipper.
    public FocusZipper<X> leftCycle() {
        return this;
    }

    // Ex 4:
    // Create a new zipper focused one element to the right;
    // If there are no more elements, cycle around to the
    // front of the zipper.
    public FocusZipper<X> rightCycle() {
        return this;
    }

    // Ex 5:
    // Count the elements of this zipper
    public Integer count() {
        return 0;
    }

    // Ex 6:
    // Focus on the nth element to the right from this zipper
    public FocusZipper<X> nth(Integer n) {
        return this;
    }

    // Ex 7:
    // Focus on a randomly chosen element of this zipper
    public FocusZipper<X> random() {
        return this;
    }

    // Creates a string representation of this zipper
    public String toString() {
        return "⟨" + context + ", " + focus + ", " + rest + "⟩";
    }

}