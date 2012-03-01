// Compute leftmost element of binary tree of X.
public class LeftMost<X> implements IVisitorBT<X,X> {
    public X visitLeaf() { throw new RuntimeException("No leftmost."); }
    public X visitNode(X val, IBT<X> left, IBT<X> right) {
	return left.accept(new LeftMostAcc<X>(val));
    }
}
