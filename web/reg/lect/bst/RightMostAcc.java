// Compute rightmost element of binary tree of X.
// Accumulator: rightmost X seen so far.
public class RightMostAcc<X> implements IVisitorBT<X,X> {
    X seen;
    RightMostAcc(X seen) {
	this.seen = seen;
    }

    public X visitLeaf() { return this.seen; }
    public X visitNode(X val, IBT<X> left, IBT<X> right) {
	return right.accept(new RightMostAcc<X>(val));
    }
}
