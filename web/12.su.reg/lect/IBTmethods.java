import tester.*;

// Interp: represents a binary tree with no information in the nodes
// and integer data in the leaves
interface IBTmethods {    
    // count the leaves in this binary tree
    public Integer countLeaves();
    
    // are the leaf values ordered from left to right?
    public Boolean orderedHuh();
    
    // find the value of the rightmost leaf in this tree
    public Integer rightMost();
    
    // find the value of the leftmost leaf in this tree
    public Integer leftMost();    

    // compute maximal element of this tree.
    public Integer max();

    // compute minimal element of this tree.
    public Integer min();    
}

// to represent a leaf of a binary tree
class Leaf implements IBTmethods{
    Integer data;
    
    Leaf(Integer data){
	this.data = data;
    }
    
    /* TEMPLATE
     * FIELDS:
     * ... this.data ...                    -- Integer
     * METHODS:
     * ... this.countLeaves() ...           -- Integer
     * ... this.orderedHuh() ...            -- Boolean
     * ... this.rightMost() ...             -- Integer
     * ... this.leftMost() ...              -- Integer
     */

    // count the leaves in this leaf
    public Integer countLeaves() {
	return 1;
    }  
    
    // are the leaf values ordered from left to right?
    public Boolean orderedHuh() {
	return true;
    }
    
    // find the value of the rightmost leaf in this leaf
    public Integer rightMost() {
	return this.data;
    }
    
    // find the value of the leftmost leaf in this leaf
    public Integer leftMost(){
	return this.data;
    }

    // compute maximal element of this leaf.
    public Integer max() {
	return this.data;
    }

    // compute minimal element of this leaf.
    public Integer min() {
	return this.data;
    }    
}

// to represent a node in a binary tree
class Node implements IBTmethods {
    IBTmethods left;
    IBTmethods right;
    
    Node(IBTmethods left, IBTmethods right){
	this.left = left;
	this.right = right;
    }
    
    /* TEMPLATE
     * FIELDS:
     * ... this.left ...                    -- IBT
     * ... this.right ...                   -- IBT
     * METHODS:
     * ... this.countLeaves() ...           -- Integer
     * ... this.orderedHuh() ...            -- Boolean
     * ... this.rightMost() ...             -- Integer
     * ... this.leftMost() ...              -- Integer
     * METHODS FOR FIELDS:
     * ... this.left.countLeaves() ...      -- Integer
     * ... this.right.countLeaves() ...     -- Integer
     * ... this.left.orderedHuh() ...       -- Boolean
     * ... this.right.orderedHuh() ...      -- Boolean
     * ... this.left.rightMost() ...        -- Integer
     * ... this.right.rightMost() ...       -- Integer
     * ... this.left.leftMost() ...         -- Integer
     * ... this.right.leftMost() ...        -- Integer
     */
    
    // count the leaves in this binary tree
    public Integer countLeaves(){
	return this.left.countLeaves() + right.countLeaves();
    }
    
    // are the leaf values ordered from left to right?
    public Boolean orderedHuh(){
	return this.left.orderedHuh() && 
	    this.right.orderedHuh() &&
	    (this.left.rightMost() <= this.right.leftMost());
    }
    
    // find the value of the rightmost leaf in this tree
    public Integer rightMost(){
	return this.right.rightMost();
    }
    
    // find the value of the leftmost leaf in this tree
    public Integer leftMost(){
	return this.left.leftMost();
    }

    // compute maximal element of this node.
    public Integer max() {
	return Math.max(this.left.max(), this.right.max());
    }

    // compute minimal element of this node.
    public Integer min() {
	return Math.min(this.left.min(), this.right.min());
    }
}

// Examples for binary trees
class ExamplesBTs{
    ExamplesBTs(){}
    
    IBTmethods l3 = new Leaf(3);
    IBTmethods l5 = new Leaf(5);
    IBTmethods l2 = new Leaf(2);
    IBTmethods l4 = new Leaf(4);
    IBTmethods l6 = new Leaf(6);
    IBTmethods l8 = new Leaf(8);
    
 /*
       tree              leftTree         rightTree
     /      \            /      \         /       \
    3      /  \         2        3       /\        8
          5    3                        4  6
          
       bigTree
     /         \
   /  \       /  \
  2    3     /\   8
            4  6
  */
  
    IBTmethods tree = new Node(this.l3, new Node(this.l5, this.l3));
    IBTmethods leftTree = new Node(this.l2, this.l3);
    IBTmethods rightTree = new Node(new Node(this.l4, this.l6), this.l8);
    IBTmethods bigTree = new Node(this.leftTree, this.rightTree);
    
    // test the method countLeaves for the union of classes IBT
    Boolean testCountLeaves(Tester t){
	return
	    t.checkExpect(this.l3.countLeaves(), 1) &&
	    t.checkExpect(this.tree.countLeaves(), 3);
    }  
    
    // test the method orderedHuh for the union of classes IBT
    Boolean testOrderedHuh(Tester t){
	return
	    t.checkExpect(this.l3.orderedHuh(), true) &&
	    t.checkExpect(this.tree.orderedHuh(), false) &&
	    t.checkExpect(this.bigTree.orderedHuh(), true);
    }  
    
    // test the method leftMost for the union of classes IBT
    Boolean testLeftMost(Tester t){
	return
	    t.checkExpect(this.l3.leftMost(), 3) &&
	    t.checkExpect(this.tree.leftMost(), 3) &&
	    t.checkExpect(this.rightTree.leftMost(), 4) &&
	    t.checkExpect(this.bigTree.leftMost(), 2);
    }
    
    // test the method rightMost for the union of classes IBT
    Boolean testRightMost(Tester t){
	return
	    t.checkExpect(this.l3.rightMost(), 3) &&
	    t.checkExpect(this.tree.rightMost(), 3) &&
	    t.checkExpect(this.rightTree.rightMost(), 8) &&
	    t.checkExpect(this.bigTree.rightMost(), 8);
    }

    Boolean testMinMax(Tester t) {
	return
	    t.checkExpect(this.bigTree.min(), 2) &&
	    t.checkExpect(this.bigTree.max(), 8);
    }
}
