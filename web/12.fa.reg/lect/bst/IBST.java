// Represents a binary search tree of X.
public interface IBST<X> extends IBT<X> {
    // Get the smallest element in this tree.
    X min();
    // Get the largest element in this tree.
    X max();

    // Insert given element into this binary search tree.
    IBST<X> insert(X x);
}
