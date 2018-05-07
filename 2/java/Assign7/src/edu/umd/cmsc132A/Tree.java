package edu.umd.cmsc132A;

import java.util.Iterator;

// Visitor for trees
interface TreeVisitor<X, R> {
    R visitLeaf(Leaf<X> leaf);

    R visitNode(Node<X> node);
}

interface Tree<X> extends Iterable<X> {
    <R> R accept(TreeVisitor<X, R> visitor);
}

class Leaf<X> implements Tree<X> {
    public <R> R accept(TreeVisitor<X, R> visitor) {
        return visitor.visitLeaf(this);
    }

    public Iterator<X> iterator() {
        return new TreeIterator<>(new LeafTraversal<X>());
    }
}

class Node<X> implements Tree<X> {
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

    // Use NLR iterator by default
    public Iterator<X> iterator() {
        return new TreeIterator<>(new NLR<>(this));
    }
}

// Wrapper class to implement iterator with (functional) traversal
class TreeIterator<X> implements Iterator<X> {
    TreeTraversal<X> tr;

    TreeIterator(TreeTraversal<X> tr) {
        this.tr = tr;
    }

    public boolean hasNext() {
        return this.tr.has();
    }

    public X next() {
        X x = this.tr.get();
        this.tr = tr.next();
        return x;
    }
}

// Functional tree traversal interface
interface TreeTraversal<X> extends Iterable<X> {
    // Advance the traversal (if possible)
    TreeTraversal<X> next();

    // Get the current element in this traversal (if there is one)
    X get();

    // Is there a current element in this traversal?
    Boolean has();
}

// Visit a Tree and produce a traversal
class MakeNLR<X> implements TreeVisitor<X, TreeTraversal<X>> {
    public TreeTraversal<X> visitNode(Node<X> node) {
        return new NLR<>(node);
    }

    public TreeTraversal<X> visitLeaf(Leaf<X> leaf) {
        return new LeafTraversal<>();
    }
}

abstract class ATreeTraversal<X> implements TreeTraversal<X> {
    public Iterator<X> iterator() { return new TreeIterator<>(this); }
}

// Leaf (i.e. empty tree) traversal
class LeafTraversal<X> extends ATreeTraversal<X> {
    public TreeTraversal<X> next() {
        throw new RuntimeException("no more elements");
    }

    public X get() {
        throw new RuntimeException("no element");
    }

    public Boolean has() {
        return false;
    }
}

// Pre-order traversal
// Interp: node is the current node; context is list of
//         nodes still to be visited
class NLR<X> extends ATreeTraversal<X> {
    Node<X> node;
    Listof<Node<X>> context;

    NLR(Node<X> node) {
        this(node, new Empty<>());
    }

    NLR(Node<X> node, Listof<Node<X>> context) {
        this.node = node;
        this.context = context;
    }

    public TreeTraversal<X> next() {
        return this.node.accept(new NLRNext<>(this.context));
    }

    public X get() {
        return this.node.value;
    }

    public Boolean has() {
        return true;
    }
}

// Interface for any computation over a Node
// that needs to do case analysis on both subtrees
interface NodeChildVisitor<X, R> extends TreeVisitor<X, R> {
    // Both are leafs
    R visitLeafLeaf();

    // Left is a leaf, right is node
    R visitLeafNode(Node<X> right);

    // Left is a node, right is a leaf
    R visitNodeLeaf(Node<X> left);

    // Both are nodes
    R visitNodeNode(Node<X> left, Node<X> right);
}

// Visit a node to advance to the next traversal (in NLR order)
class NLRNext<X> extends ANodeChildVisitor<X, TreeTraversal<X>> {
    Listof<Node<X>> context;

    NLRNext(Listof<Node<X>> context) {
        this.context = context;
    }

    // Both are leafs:
    // look to the context
    public TreeTraversal<X> visitLeafLeaf() {
        return this.context.accept(
                new ListVisitor<Node<X>, TreeTraversal<X>>() {
            public TreeTraversal<X> visitEmpty(Empty<Node<X>> empty) {
                return new LeafTraversal<>();
            }

            public TreeTraversal<X> visitCons(Cons<Node<X>> cons) {
                return new NLR<>(cons.first, cons.rest);
            }
        });
    }

    // Left is a leaf, right is node:
    // right becomes focus, context unchanged
    public TreeTraversal<X> visitLeafNode(Node<X> right) {
        return new NLR<>(right, this.context);
    }

    // Left is a node, right is a leaf:
    // left becomes focus, context unchanged
    public TreeTraversal<X> visitNodeLeaf(Node<X> left) {
        return new NLR<>(left, this.context);
    }

    // Both are nodes:
    // left becomes focus, right added to context
    public TreeTraversal<X> visitNodeNode(Node<X> left, Node<X> right) {
        return new NLR<>(left, new Cons<>(right, this.context));
    }
}

// Abstract class for NodeChildVisitor that implements the
// TreeVisitor interface in terms of NodeChildVisitor
abstract class ANodeChildVisitor<X, R> implements NodeChildVisitor<X, R> {
    public R visitLeaf(Leaf<X> leaf) {
        throw new RuntimeException("Can't visit a leaf");
    }

    // Implements a TreeVisitor based on the four cases of
    // subtrees for node
    public R visitNode(Node<X> node) {
        return node.left.accept(new TreeVisitor<X, R>() {
            public R visitLeaf(Leaf<X> leaf) {
                return node.right.accept(new TreeVisitor<X, R>() {
                    public R visitLeaf(Leaf<X> leaf) {
                        return ANodeChildVisitor.this.visitLeafLeaf();
                    }

                    public R visitNode(Node<X> right) {
                        return ANodeChildVisitor.this.visitLeafNode(right);
                    }
                });
            }

            public R visitNode(Node<X> left) {
                return node.right.accept(new TreeVisitor<X, R>() {
                    public R visitLeaf(Leaf<X> leaf) {
                        return ANodeChildVisitor.this.visitNodeLeaf(left);
                    }

                    public R visitNode(Node<X> right) {
                        return ANodeChildVisitor.this.visitNodeNode(left, right);
                    }
                });
            }
        });
    }
}






















