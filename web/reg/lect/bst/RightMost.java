// Compute rightmost element of binary tree of X.
public class RightMost<X> implements IVisitorBT<X,X> {
    public X visitLeaf() { throw new RuntimeException("No rightmost."); }
    public X visitNode(X val, IBT<X> left, IBT<X> right) {
	return right.accept(new RightMostAcc<X>(val));
    }
}
