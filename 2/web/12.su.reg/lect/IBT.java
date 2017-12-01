import tester.*;

// to represent a binary tree with no information in the nodes
// and integer data in the leaves
interface IBT {}

// to represent a leaf of a binary tree
class Leaf implements IBT{
    Integer data;
    
    Leaf(Integer data) {
	this.data = data;
    }
}

// to represent a node in a binary tree
class Node implements IBT {
    IBT left;
    IBT right;
    
    Node(IBT left, IBT right){
	this.left = left;
	this.right = right;
    }
}

// Examples for binary trees
class ExamplesBTs{
    ExamplesBTs(){}
    
    IBT l3 = new Leaf(3);
    IBT l5 = new Leaf(5);
    IBT tree = new Node(this.l3, new Node(this.l5, this.l3));
}
