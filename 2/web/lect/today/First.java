import tester.*;

class Point {
    Integer x;
    Integer y;
    Point(Integer x, Integer y) {
	this.x = x;
	this.y = y;
    }

    // Move this point to the left one unit
    public Point moveLeft() {
	return new Point(this.x-1, this.y);
    }
}

interface BT {
    // Sum the elements of this binary tree
    public Integer sum();

    // Are the element of this binary tree are larger
    // than given number?
    public Boolean allLarger(Integer n);

    // Compute the largest element of this binary tree
    public Integer max();
}

class Leaf implements BT {
    Integer val;
    Leaf(Integer val) {
	this.val = val;
    }

    // Sum the elements of this leaf
    public Integer sum() {
	return this.val;
    }

    // Are the elements of this leaf larger
    // than given number?
    public Boolean allLarger(Integer n) {
	return this.val > n;
    }

    // Compute the largest element of this leaf
    public Integer max() {
	return this.val;
    }
}

class Node implements BT {
    BT left;
    BT right;
    Node(BT left, BT right) {
	this.left = left;
	this.right = right;
    }

    // Sum the elements of this node
    public Integer sum() {
	return this.left.sum()
	    + this.right.sum();
    }

    // Are all the elements of this node larger than given
    // number?
    public Boolean allLarger(Integer n) {
	return this.left.allLarger(n)
	    && this.right.allLarger(n);
    }

    // Compute the largest element of this node
    public Integer max() {
	Integer maxLeft = this.left.max();
	Integer maxRight = this.right.max();
	if (maxLeft > maxRight) {
	    return maxLeft;
	} else {
	    return maxRight;
	}
    }
}

class FirstExamples {

    BT l5 = new Leaf(5);
    BT l7 = new Leaf(7);
    BT n57 = new Node(l5, l7);

    void testFirst(Tester t) {
	t.checkExpect(new Point(3, 4), new Point(3, 4));
	t.checkExpect(new Point(3, 5).moveLeft(),
		      new Point(2, 5));
	t.checkExpect(true, true);
    }

    void testSum(Tester t) {
	t.checkExpect(new Leaf(5).sum(), 5);
	t.checkExpect(new Node(new Leaf(5), new Leaf(7)).sum(),
		      12);
    }

    void testAllLarger(Tester t) {
	t.checkExpect(l5.allLarger(2), true);
	t.checkExpect(l5.allLarger(7), false);
	t.checkExpect(n57.allLarger(6), false);
	t.checkExpect(n57.allLarger(7), false);
	t.checkExpect(n57.allLarger(3), true);
    }

    void testMax(Tester t) {
	t.checkExpect(l5.max(), 5);
	t.checkExpect(l7.max(), 7);
	t.checkExpect(n57.max(), 7);
    }
}
