import java.util.*;
import tester.*;

interface BST<X> extends Iterable<X> {
    // Insert given element into this BST.
    BST<X> insert(X x);

    // Add all elements of this BST to ls, in order.
    void addTo(ArrayList<X> ls);
}

abstract class ABST<X> implements BST<X> {
    Comparator<X> c;
    ABST(Comparator<X> c) {
	this.c = c;
    }

    public Iterator<X> iterator() {
	ArrayList<X> elems = new ArrayList<X>();
	this.addTo(elems);
	return elems.iterator();
    }
}

class Leaf<X> extends ABST<X> {
    Leaf(Comparator<X> c) {
	super(c);
    }

    public BST<X> insert(X x) {
	return new Node<X>(this.c, x, this, this);
    }

    public void addTo(ArrayList<X> ls) {}
}

class Node<X> extends ABST<X> {
    X x;
    BST<X> left;
    BST<X> right;
    Node(Comparator<X> c, X x, BST<X> left, BST<X> right) {
	super(c);
	this.x = x;
	this.left = left;
	this.right = right;
    }

    public BST<X> insert(X x) {
	if (this.c.compare(x, this.x) < 0) {
	    return new Node<X>(this.c, this.x, this.left.insert(x), this.right);
	} else {
	    return new Node<X>(this.c, this.x, this.left, this.right.insert(x));
	}
    }

    public void addTo(ArrayList<X> ls) {
	this.left.addTo(ls);
	ls.add(this.x);
	this.right.addTo(ls);
    }
}

class Sort {
    <X> ArrayList<X> treeSort(Comparator<X> c, Iterable<X> elems) {
	BST<X> bst = new Leaf<X>(c);
	ArrayList<X> ls = new ArrayList<X>();
	for (X x : elems) bst = bst.insert(x);
	for (X x : bst)   ls.add(x);
	return ls;
    }
}

class LessThan implements Comparator<Integer> {
    public int compare(Integer x, Integer y) {
	return x - y;
    }
}

class Examples {
    void testTreeSort(Tester t) {
	Sort s = new Sort();
	Comparator<Integer> lt = new LessThan();
	ArrayList<Integer> elems =
	    new ArrayList<Integer>(Arrays.asList(8,3,7,5));
	
	ArrayList<Integer> sorted =
	    new ArrayList<Integer>(Arrays.asList(3,5,7,8));

	t.checkExpect(s.treeSort(lt, new ArrayList<Integer>()),
		      new ArrayList<Integer>());
	t.checkExpect(s.treeSort(lt, elems), sorted);
    }
}