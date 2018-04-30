package edu.umd.cmsc132A;

@FunctionalInterface
interface TriFunction<T, U, V, R> {
    // Apply this ternary function
    R apply(T t, U u, V v);
}

interface TreeVisitor<X, R> {
    R visitLeaf(Leaf<X> leaf);
    R visitNode(Node<X> node);
}

interface Tree<X> {
    <R> R fold(TriFunction<X, R, R, R> f, R b);
    <R> R accept(TreeVisitor<X, R> visitor);
}

class Leaf<X> implements Tree<X> {
    public <R> R fold(TriFunction<X, R, R, R> f, R b) {
        return b;
    }

    public <R> R accept(TreeVisitor<X, R> visitor) {
        return visitor.visitLeaf(this);
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

    public <R> R fold(TriFunction<X, R, R, R> f, R b) {
        return f.apply(
                this.value,
                this.left.fold(f, b),
                this.right.fold(f, b));
    }

    public <R> R accept(TreeVisitor<X, R> visitor) {
        return visitor.visitNode(this);
    }
}