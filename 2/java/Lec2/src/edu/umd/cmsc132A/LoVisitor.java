package edu.umd.cmsc132A;

interface LoVisitor<X,R> {
    R visitEmpty();
    R visitCons(X first, Lo<X> rest);
}
