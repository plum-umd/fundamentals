// Represents a leaf in a binary tree of X.
public class Leaf<X> extends ABT<X> {
    Leaf() {}

    public <T> T accept(IVisitorBT<X,T> v) {
	return v.visitLeaf();
    }
}
