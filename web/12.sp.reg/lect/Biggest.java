import java.util.Comparator;

// Find the largest element in the visited tree.
public class Biggest<X> implements IVisitorBT<X,X> {
    Comparator<X> comp;
    Biggest(Comparator<X> comp) {
	this.comp = comp;
    }

    public X visitLeaf() { throw new RuntimeException("No elements in tree."); }
    public X visitNode(X val, IBT<X> left, IBT<X> right) {
	X biggestLeft = left.accept(new BiggestAcc<X>(val));
	X biggestRight = right.accept(new BiggestAcc<X>(val));
	
	if (comp.compare(biggestLeft, biggestRight) < 0) {
	    return biggestRight;
	} else {
	    return biggestLeft;
	}
    }
}
