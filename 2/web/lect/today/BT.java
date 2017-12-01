import tester.*;

interface BT {
    // Duplicate this binary tree w/ a node
    BT duplicate();

    // Sum the elements of this binary tree
    Integer sum();
}

abstract class ABT implements BT {
    public BT duplicate() {
	return new Node(this, this);
    }
}

class Leaf extends ABT {
    Integer val;
    Leaf(Integer val) {
	this.val = val;
    }

    public Integer sum() {
	return this.val;
    }

    public BT duplicate() {
	return this;
    }
}

class Node extends ABT {
    BT left;
    BT right;
    Node(BT left, BT right) {
	this.left = left;
	this.right = right;
    }

    public Integer sum() {
	return this.left.sum()
	    + this.right.sum();
    }
}

class Examples {
    BT l5 = new Leaf(5);
    BT n55 = new Node(l5, l5);
    BT n5555 = new Node(n55, n55);

    void testDuplicate(Tester t) {
	t.checkExpect(l5.duplicate(), n55);
	t.checkExpect(n55.duplicate(), n5555);
    }
    void testSum(Tester t) {
	t.checkExpect(l5.sum(), 5);
	t.checkExpect(n5555.sum(), 20);
    }
}