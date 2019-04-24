package edu.umd.cmsc132A;

class Pair<X,Y> {
    X left;
    Y right;

    Pair(X left, Y right) {
        this.left = left;
        this.right = right;
    }

    // Swap the elements in this pair
    Pair<Y,X> swap() {
        return new Pair<>(this.right, this.left);
    }
}
