package edu.umd.cmsc132A;

// Random-access list

import tester.Tester;

import java.util.Optional;

class RaLo<X> {

   Lo<Node<X>> trees;

    RaLo() {
        this.trees = new Empty<>();
    }

    private RaLo(Lo<Node<X>> trees) {
        this.trees = trees;
    }

    X get(Integer i) {
        return this.trees.accept(new LoVisitor<>() {
            public X visitEmpty() {
                throw new RuntimeException("index too large for list");
            }

            public X visitCons(Node<X> first, Lo<Node<X>> rest) {
                return i < first.size() ?
                        first.get(i) :
                        new RaLo<>(rest).get (i - first.size());
            }
        });
    }

    RaLo<X> cons(X x) {
        // If trees is empty,
        //    trees contains one element, or
        //    trees begins with two trees of different sizes,
        //    cons a singleton tree
        // If trees begins with two trees of same size,
        //    form a node with x.
        return new RaLo<>(trees.accept(new LoVisitor<>() {
            public Lo<Node<X>> visitEmpty() {
                return new Cons<>(FBT.single(x), trees);
            }

            public Lo<Node<X>> visitCons(Node<X> first, Lo<Node<X>> rest) {
                return rest.accept(new LoVisitor<>() {
                    public Lo<Node<X>> visitEmpty() {
                        return new Cons<>(FBT.single(x), trees);
                    }

                    public Lo<Node<X>> visitCons(Node<X> second, Lo<Node<X>> rest2) {
                        return (first.size().equals(second.size())) ?
                                new Cons<>(new Node<>(x, first, second), rest2) :
                                new Cons<>(FBT.single(x), trees);
                    }
                });
            }
        }));
    }

    Optional<X> first() {
        return this.trees.accept(new LoVisitor<Node<X>, Optional<X>>() {
            @Override
            public Optional<X> visitEmpty() {
                return Optional.empty();
            }

            @Override
            public Optional<X> visitCons(Node<X> first, Lo<Node<X>> rest) {
                return Optional.of(first.val);
            }
        });
    }

    Optional<RaLo<X>> rest() {
        
    }
}

// A "Full" Binary Tree
interface FBT<X> {
    X get(Integer i);

    Integer size();

    static <X> Node<X> single(X x) {
        return new Node<>(x, new Leaf<>(), new Leaf<>());
    }
}

class Leaf<X> implements FBT<X> {
    public X get(Integer i) {
        throw new RuntimeException("Index too large for this tree");
    }

    public Integer size() { return 0; }
}


// Invariant: left and right subtrees are FBT of the same height
class Node<X> implements FBT<X> {
    X val;
    private final FBT<X> left;
    private final FBT<X> right;

    Node(X val, FBT<X> left, FBT<X> right) {
        this.val = val;
        this.left = left;
        this.right = right;
    }

    public X get(Integer i) {
        Integer j = ((this.size() - 1) / 2);
        return (i == 0) ? this.val : (i <= j) ?
                this.left.get(i - 1) :
                this.right.get(i - j - 1);
    }

    public Integer size() {
        return 1 + 2 * this.left.size();
    }

}

class TestRaLo {

    void testRaLo(Tester t) {
        /*
        FBT<Integer> l = new Leaf<>();
        FBT<Integer> one = new Node<>(7, l, l);
        FBT<Integer> three = new Node<>(9, one, new Node<>(1, l, l));
        FBT<Integer> seven = new Node<>(12, three,
                new Node<>(8, new Node<>(1, l, l), new Node<>(4, l, l)));
        */

        // RaLo<Integer> ls = new RaLo<>(new Cons<>(seven, new Cons<>(seven, new Empty<>())));
        RaLo<Integer> ls = new RaLo<>();
        ls = ls.cons(4).cons(1).cons(8).cons(1).cons(7).cons(9).cons(12);
        ls = ls.cons(4).cons(1).cons(8).cons(1).cons(7).cons(9).cons(12);

        t.checkExpect(ls.get(0), 12);
        t.checkExpect(ls.get(1), 9);
        t.checkExpect(ls.get(6), 4);
        t.checkExpect(ls.get(7), 12);
        t.checkExpect(ls.get(13), 4);
    }


    void testFBT(Tester t) {

        // FBTs represent collections of size:
        // 0, 1, 3, 7, 15, 31, 63, 127, ...

        // 2^h - 1

        FBT<Integer> l = new Leaf<>();
        FBT<Integer> one = new Node<>(7, l, l);
        FBT<Integer> three = new Node<>(9, one, new Node<>(1, l, l));
        FBT<Integer> seven = new Node<>(12, three,
                new Node<>(8, new Node<>(1, l, l), new Node<>(4, l, l)));

        t.checkExpect(one.get(0), 7);

        t.checkExpect(three.get(0), 9);
        t.checkExpect(three.get(1), 7);
        t.checkExpect(three.get(2), 1);

        t.checkExpect(seven.get(0), 12);
        t.checkExpect(seven.get(1), 9);
        t.checkExpect(seven.get(2), 7);
        t.checkExpect(seven.get(3), 1);
        t.checkExpect(seven.get(4), 8);
        t.checkExpect(seven.get(5), 1);
        t.checkExpect(seven.get(6), 4);
    }
}





