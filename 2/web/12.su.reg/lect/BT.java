import tester.*;

// Interp: An IBT represents a binary tree of
// integers with data only at the leaves.
interface IBT {
    // Count the number of leaves in this tree.
    Integer countLeaves();

    // Is this tree in non-decreasing order from left to right?    
    Boolean isOrdered();

    // Get the value of the leftmost leaf of this tree.
    Integer leftMost();
    
    // Get the value of the rightmost leaf of this tree.
    Integer rightMost();

    // Insert the given number into this ordered tree.
    IBT insert(Integer n);
}
    
class Leaf implements IBT {
    Integer data;
    Leaf(Integer data) {
	this.data = data;
    }

    // TEMPLATE
    // FIELDS:
    // this.data     -- Integer
    // METHODS:
    // this.countLeaves()    -- Integer
    // this.isOrdered()      -- Boolean
    // this.leftMost()       -- Integer
    // this.rightMost()      -- Integer

    // Count the number of leaves in this leaf.
    public Integer countLeaves() {
	return 1;
    }
    
    // Is this leaf in non-decreasing order from left to right?
    public Boolean isOrdered() {
	return true;
    }

    // Get the value of the leftmost leaf of this leaf.
    public Integer leftMost() {
	return this.data;
    }

    // Get the value of the rightmost leaf of this leaf.
    public Integer rightMost() {
	return this.data;
    }

    // Insert the given number into this ordered leaf.
    public IBT insert(Integer n) {
	if (this.data <= n) {
	    return new Node(this, new Leaf(n));
	} else {
	    return new Node(new Leaf(n), this);
	}
    }
}

class Node implements IBT {
    IBT left;
    IBT right;
    Node(IBT left, IBT right) {
	this.left = left;
	this.right = right;
    }

    // TEMPLATE
    // FIELDS:
    // this.left     -- IBT
    // this.right    -- IBT
    // METHODS:
    // this.countLeaves()        -- Integer
    // this.isOrdered()          -- Boolean
    // this.leftMost()           -- Integer
    // this.rightMost()          -- Integer    
    // METHODS FOR FIELDS:
    // this.left.countLeaves()   -- Integer
    // this.right.countLeaves()  -- Integer
    // this.left.isOrdered()     -- Boolean
    // this.right.isOrdered()    -- Boolean
    // this.left.leftMost()      -- Integer
    // this.left.rightMost()     -- Integer
    // this.right.leftMost()     -- Integer
    // this.right.rightMost()    -- Integer

    // Count the number of leaves in this node.
    public Integer countLeaves() {
	return this.left.countLeaves()
	    + this.right.countLeaves();
    }

    // Is this node in non-decreasing order from left to right?
    public Boolean isOrdered() {
	return this.left.isOrdered() 
	    && this.right.isOrdered()
	    && this.left.rightMost() <= this.right.leftMost();
    }

    // Get the value of leftmost leaf of this node.
    public Integer leftMost() {
	return this.left.leftMost();
    }
    
    // Get the value of the rightmost leaf of this node.
    public Integer rightMost() {
	return this.right.rightMost();
    }

    // Insert the given number into this ordered node.
    public IBT insert(Integer n) {
	if (n <= this.left.rightMost()) {
	    return new Node(this.left.insert(n), this.right);
	} else {
	    return new Node(this.left, this.right.insert(n));
	}
    }
}

class Examples {
    Examples() {}

    IBT leafFour = new Leaf(4);
    IBT nodeFiveSix = new Node(new Leaf(5), new Leaf(6));
    IBT dblFiveSix = new Node(nodeFiveSix, nodeFiveSix);

    public Boolean testCountLeaves(Tester t) {
	return t.checkExpect(leafFour.countLeaves(), 1)
	    && t.checkExpect(nodeFiveSix.countLeaves(), 2);	    
    }

    public Boolean testLeftMost(Tester t) {
	return t.checkExpect(leafFour.leftMost(), 4)
	    && t.checkExpect(nodeFiveSix.leftMost(), 5);
    }

    public Boolean testRightMost(Tester t) {
	return t.checkExpect(leafFour.rightMost(), 4)
	    && t.checkExpect(nodeFiveSix.rightMost(), 6);
    }

    public Boolean testIsOrdered(Tester t) {
	return t.checkExpect(leafFour.isOrdered(), true)
	    && t.checkExpect(nodeFiveSix.isOrdered(), true)
	    && t.checkExpect(dblFiveSix.isOrdered(), false);
    }
}

