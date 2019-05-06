package edu.umd.cmsc132A;

import java.util.Iterator;

// Visitor for trees
interface TreeVisitor<X, R> {
    R visitLeaf(Leaf<X> leaf);
    R visitNode(Node<X> node);
}

interface Tree<X> extends Iterable<X> {
    <R> R accept(TreeVisitor<X, R> visitor);

    // Produce post- and in-order iterators
    Iterator<X> iteratorPostOrder();
    Iterator<X> iteratorInOrder();
    Iterator<X> iteratorPreOrder();
}

abstract class ATree<X> implements Tree<X> {
    // Produce in-order iterator by default
    public Iterator<X> iterator() {
        return this.iteratorPreOrder();
    }

    public Iterator<X> iteratorInOrder() {
        return null; // stubbed.
    }

    public Iterator<X> iteratorPostOrder() {
        return null; // stubbed
    }

    public Iterator<X> iteratorPreOrder() {
        return new PreOrderIterator<>(this);
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

// Implements a pre-order traversal of a binary tree
class PreOrderIterator<X> implements Iterator<X> {

    Tree<X> curr;
    Listof<Node<X>> rights;

    PreOrderIterator(Tree<X> curr) {
        this.curr = curr;
        this.rights = new Empty<>();
    }

    public X next() {

        return this.curr.accept(new TreeVisitor<X, X>() {
            public X visitLeaf(Leaf<X> leaf) {
                return rights.accept(new ListVisitor<Node<X>, X>() {
                    public X visitEmpty(Empty<Node<X>> mt) {
                        throw new RuntimeException("No next element");
                    }

                    public X visitCons(Cons<Node<X>> cons) {
                        curr = cons.first.left;
                        rights = // ...if cons.first.right is a leaf, rights, otherwise cons cons.first.right on...
                           cons.first.right.accept(new TreeVisitor<X, Listof<Node<X>>>() {
                               public Listof<Node<X>> visitLeaf(Leaf<X> leaf) {
                                   return cons.rest;
                               }

                               public Listof<Node<X>> visitNode(Node<X> node) {
                                   return new Cons<>(node, cons.rest);
                               }
                           });
                        return cons.first.value;
                    }
                });
            }

            public X visitNode(Node<X> node) {
                curr = node.left;
                rights = node.right.accept(new TreeVisitor<X, Listof<Node<X>>>() {
                    public Listof<Node<X>> visitLeaf(Leaf<X> leaf) {
                        return rights;
                    }

                    public Listof<Node<X>> visitNode(Node<X> node) {
                        return new Cons<>(node, rights);
                    }
                });
                return node.value;
            }
        });
    }

    public boolean hasNext() {
        // there's a next item if curr is a node
        // or rights is non-empty.
        return this.curr.accept(new TreeVisitor<>() {
            public Boolean visitLeaf(Leaf<X> leaf) {
                return rights.accept(new ListVisitor<>() {
                    public Boolean visitEmpty(Empty<Node<X>> mt) {
                        return false;
                    }

                    public Boolean visitCons(Cons<Node<X>> cons) {
                        return true;
                    }
                }) ;
            }

            public Boolean visitNode(Node<X> node) {
                return true;
            }
        });
    }
}

















