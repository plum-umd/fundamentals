public class Size<X> implements IVisitorBT<X,Integer> {
    public Integer visitLeaf() {
	return 0;
    }

    public Integer visitNode(X val, IBT<X> left, IBT<X> right) {
	return 1 + left.accept(this) + right.accept(this);
    }
}
