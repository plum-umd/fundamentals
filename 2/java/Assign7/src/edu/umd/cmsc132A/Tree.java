package edu.umd.cmsc132A;

import java.util.Iterator;

// Visitor for trees
interface TreeVisitor<X, R> {
    R visitLeaf(Leaf<X> leaf);
    R visitNode(Node<X> node);
}

interface Tree<X> extends Iterable<X> {
    <R> R accept(TreeVisitor<X, R> visitor);

    // Produce pre- and in-order iterators
    Iterator<X> iteratorPreOrder();
    Iterator<X> iteratorInOrder();
}

abstract class ATree<X> {
    // Produce in-order iterator by default
    public Iterator<X> iterator() {
        return this.iteratorInOrder();
    }

    public Iterator<X> iteratorInOrder() {
        return null; // stubbed.
    }

    public Iterator<X> iteratorPreOrder() {
        return null; // stubbed
    }
}

class Leaf<X> extends ATree<X> {
    public <R> R accept(TreeVisitor<X, R> visitor) {
        return visitor.visitLeaf(this);
    }
}

class Node<X> extends ATree<X> {
    X value;
    Tree<X> left;
    Tree<X> right;

    Node(X value, Tree<X> left, Tree<X> right) {
        this.value = value;
        this.left = left;
        this.right = right;
    }

    public <R> R accept(TreeVisitor<X, R> visitor) {
        return visitor.visitNode(this);
    }
}
















