import java.util.*;

// Represents the insert of x into a sorted (by c) BT of X
public class Insert<X> implements IVisitorBT<X, IBT<X>> {
    Comparator<X> c;
    X x;

    Insert(Comparator<X> c, X x) {
	this.c = c;
	this.x = x;
    }

    public IBT<X> visitLeaf() {
	return new Node<X>(this.x, new Leaf<X>(), new Leaf<X>());
    }

    public IBT<X> visitNode(X val, IBT<X> left, IBT<X> right) {
	if (c.compare(this.x, val) <= 0) {
	    return new Node<X>(val, left.accept(this), right);
	} else {
	    return new Node<X>(val, left, right.accept(this));
	}
    }
}
	
	