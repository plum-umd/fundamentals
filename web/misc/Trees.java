import tester.*;

/*
;; A [BT X] is one of:
;; - (new leaf% X)
;; - (new node% X [BT X] [BT X])
;; implements
;; - size : -> Natural
;;   Compute the size of this binary tree.
;; - accept : [BTVisitor X R] -> R
;;   Accept given visitor and visit data in this tree.

;; A [BTVisitor X R] implements
;; - visit-leaf : X -> R
;; - visit-node : X [BT X] [BT X] -> R
*/

/*
;; in leaf%
(define (accept v)
  (v . visit-leaf (this . val)))

;; in node%
(define (accept v)
  (v . visit-node (this . val) (this . left) (this . right)))
*/

/*
;; implements [BTVisitor X Natural].
(define-class size-visitor%
  (define (visit-leaf n) 1)
  (define (visit-node n left right) 
    (+ 1 (left . accept this) (right . accept right))))

(lseven . accept (new size-visitor%))
*/

interface BT<X> {
    // Compute the size of this binary tree.
    Integer size();

    // Compute the mirror image of this binary tree.
    BT<X> mirror();

    // Accept the given visitor and visit this tree's data.
    <R> R accept(BTVisitor<X,R> v);
}

/*
interface BTVisitor {
    Integer visitLeaf(Integer n);
    Integer visitNode(Integer n, BT left, BT right);
}

interface BTVisitorBT {
    BT visitLeaf(Integer n);
    BT visitNode(Integer n, BT left, BT right);
}
*/

// Represents a visitor on lists of Xs producing an R.
interface BTVisitor<X,R> {
    R visitLeaf(X n);
    R visitNode(X n, BT<X> left, BT<X> right);
}

// Mirrors a binary tree of Xs.
class MirrorVisitor<X> implements BTVisitor<X,BT<X>> {
    MirrorVisitor() {}
    
    public BT<X> visitLeaf(X val) { 
	return new Leaf<X>(val);
    }

    public BT<X> visitNode(X val, BT<X> left, BT<X> right) {
	return new Node<X>(val, right.accept(this), left.accept(this));
    }
}

class SizeVisitor<X> implements BTVisitor<X,Integer> {
    SizeVisitor() {}

    public Integer visitLeaf(X val) {
	return 1;
    }

    public Integer visitNode(X val, BT<X> left, BT<X> right) {
	return 1 + left.accept(this) + right.accept(this);
    }
}

abstract class SuperTree<X> implements BT<X> {
    public Integer size() {
	return this.accept(new SizeVisitor<X>());
    }
    public BT<X> mirror() {
	return this.accept(new MirrorVisitor<X>());
    }
}

class Leaf<X> extends SuperTree<X> {
    X val;
    Leaf(X val) {
	this.val = val;
    }

    public <R> R accept(BTVisitor<X,R> v) {
	return v.visitLeaf(this.val);
    }
}

class Node<X> extends SuperTree<X> {
    X val;
    BT<X> left;
    BT<X> right;
    Node(X val, BT<X> left, BT<X> right) {
	this.val = val;
	this.left = left;
	this.right = right;
    }

    public <R> R accept(BTVisitor<X,R> v) {
    	return v.visitNode(this.val, this.left, this.right);
    }
}

class Examples {
    // Some examples of binary trees of Integers.
    BT<Integer> lseven = new Leaf<Integer>(7);
    BT<Integer> lsix = new Leaf<Integer>(6);
    BT<Integer> tfoursixseven = new Node<Integer>(4, lsix, lseven);
    BT<Integer> tanother = new Node<Integer>(8, tfoursixseven, lseven);    

    // Some examples of binary trees of Strings.
    BT<String> lfred = new Leaf<String>("Fred");
    BT<String> lbob = new Leaf<String>("Bob");
    BT<String> tjanebobfred = new Node<String>("Jane", lbob, lfred);

    // Some examples of binary trees of binary trees of Strings.
    // Weird?  Nah.
    BT<BT<String>> lbfred = new Leaf<BT<String>>(lfred);
    BT<BT<String>> tbjanebobfred = 
	new Node<BT<String>>(tjanebobfred, lbfred, lbfred);
    
    Boolean testSize(Tester t) {
	return t.checkExpect(lseven.size(), 1)
	    && t.checkExpect(tanother.size(), 1 + 1 + 1 + 1 + 1)
	    && t.checkExpect(tjanebobfred.size(), 3)
	    && t.checkExpect(tbjanebobfred.size(), 3);
    }

    Boolean testMirror(Tester t) {
	return t.checkExpect(lseven.mirror(), lseven)
	    && t.checkExpect(tfoursixseven.mirror(), 
			     new Node<Integer>(4, lseven, lsix))
	    && t.checkExpect(tjanebobfred.mirror(),
			     new Node<String>("Jane", lfred, lbob));
    }

    Boolean testSizeVisitor(Tester t) {
	return t.checkExpect(lseven.accept(new SizeVisitor<Integer>()), 1)
	    && t.checkExpect(tanother.accept(new SizeVisitor<Integer>()), 5)
	    && t.checkExpect(tjanebobfred.accept(new SizeVisitor<String>()), 3);
    }
}


