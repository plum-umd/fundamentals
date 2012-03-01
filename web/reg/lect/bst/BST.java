import java.util.*;

public class BST<X> implements IBST<X> {
    Comparator<X> c;
    IBT<X> bt;

    // Public construct checks invariant
    public BST(Comparator<X> c, IBT<X> bt) {
	this(bt, c);
	if (!bt.accept(new IsSorted<X>(c))) {
	    throw new RuntimeException("Given BT is not sorted.");
	}
    }

    // Private constructor assumes invariant
    // A dirty trick: swap argument order to tell constructors apart.
    private BST(IBT<X> bt, Comparator<X> c) {
	this.c = c;
	this.bt = bt;
    }

    public X max() {
	// Take advantage of the sorted invariant
	return this.bt.accept(new RightMost<X>());
    }

    public X min() {
	// Take advantage of the sorted invariant
	return this.bt.accept(new LeftMost<X>());
    }

    public IBST<X> insert(X x) {
	// ... what to do here? 
	// Options:
	// 1) add the following method to IBT<X> interface:
	//    // Insert given element in this binary tree.
	//    // ASSUME: this binary tree is sorted by c.
	//    IBT<X> insert(Comparator<X> c, X x);
	// 2) define a class:
	//    class Insert<X> implements IVisitor<X, IBT<X>>
	//    and visit bt with new Insert<X>(c,x).
	// We go with option 2 here.

	return new BST<X>(this.bt.accept(new Insert<X>(this.c, x)), this.c);
    }

    public Integer size() {
	return this.bt.accept(new Size<X>());
    }

    public <T> T accept(IVisitorBT<X,T> v) {
	return this.bt.accept(v);
    }
}
