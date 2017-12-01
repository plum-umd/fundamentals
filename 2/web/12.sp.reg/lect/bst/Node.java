// Represents a node in a binary tree of X.
public class Node<X> extends ABT<X> {
    X val;
    IBT<X> left;
    IBT<X> right;

    Node(X val, IBT<X> left, IBT<X> right) {
	this.val = val;
	this.left = left;
	this.right = right;
    }

    public <T> T accept(IVisitorBT<X,T> v) {
	return v.visitNode(this.val, this.left, this.right);
    }
}
