// Represents a binary tree.
public interface IBT<X> {
    // Accept the given visitor and visit the data in this tree.
    public <T> T accept(IVisitorBT<X,T> v);

    // Number of values in this tree.
    Integer size();
}
