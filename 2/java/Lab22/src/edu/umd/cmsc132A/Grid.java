package edu.umd.cmsc132A;

import java.util.function.Function;

public class Grid<X> {

    // A zipper of rows of elements of type X
    FocusZipper<FocusZipper<X>> grid;

    public Grid(Listof<Listof<X>> elts) {
        this.grid = new FocusZipper<>(elts.map(FocusZipper::new));
    }

    private Grid(FocusZipper<FocusZipper<X>> grid) {
        this.grid = grid;
    }

    // Return the focused element of this grid
    public X focus() {
        return grid.focus.focus;
    }

    // Update the focused element of this grid
    public Grid<X> updateFocus(Function<X, X> f) {
        return new Grid<>(grid.updateFocus(z -> z.updateFocus(f)));
    }

    // Ex 1:
    // Move up one row in the grid
    public Grid<X> up() {
        return this;
    }

    // Ex 1:
    // Move down one row in the grid
    public Grid<X> down() {
        return this;
    }

    // Ex 2:
    // Move left one column in the grid
    public Grid<X> left() {
        return this;
    }

    // Ex 2:
    // Move right one column in the grid
    public Grid<X> right() {
        return this;
    }

    // Map the function `f' over this grid
    public <R> Grid<R> map(Function<X, R> f) {
        return new Grid<>(grid.map(z -> z.map(f)));
    }

    // Unzip this grid into a list of lists
    public Listof<Listof<X>> unzip() {
        return grid.map(FocusZipper::unzip).unzip();
    }

}
