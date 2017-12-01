import java.util.*;

public class IsSorted<X> implements IVisitorBT<X,Boolean> {
    Comparator<X> c;
    IsSorted(Comparator<X> c) {
	this.c = c;
    }

    public Boolean visitLeaf() {
	return true;
    }

    public Boolean visitNode(X val, IBT<X> left, IBT<X> right) {
	return left.accept(this)
	    && right.accept(this)
	    && c.compare(left.accept(new RightMostAcc<X>(val)), val) <= 0
	    && c.compare(val, right.accept(new LeftMostAcc<X>(val))) <= 0;
    }
}
