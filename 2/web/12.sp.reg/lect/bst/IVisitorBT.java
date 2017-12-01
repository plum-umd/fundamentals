// Represents a T computation over a binary tree of Xs.
interface IVisitorBT<X,T> {
    T visitNode(X val, IBT<X> left, IBT<X> right);
    T visitLeaf();
}
