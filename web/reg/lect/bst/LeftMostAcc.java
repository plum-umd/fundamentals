// Compute leftmost element of binary tree of X.
// Accumulator: leftmost X seen so far.
public class LeftMostAcc<X> implements IVisitorBT<X,X> {
    X seen;
    LeftMostAcc(X seen) {
	this.seen = seen;
    }

    public X visitLeaf() { return this.seen; }
    public X visitNode(X val, IBT<X> left, IBT<X> right) {
	return left.accept(new LeftMostAcc<X>(val));
    }
}

