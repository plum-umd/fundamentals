package edu.umd.cmsc132A;

import java.util.List;

/*

if I use map<x, Integer> as a field of multiset<X>. How can I convert
a multiset to list<X> (considered the number of time(value) an element(key)
repeated in the multiset)? can I have any suggestion?

 */

class ListHelper {

    // Given a list of pairs where each pair p is an element and nat. number,
    // build a list of elements p.left copied p.right times
    // Eg. buildCopies([("a", 2), ("b", 1)]) --> ["a", "a", "b"]
    public <X> Listof<X> buildCopies(Listof<Pairof<X, Integer>> ps) {
        return this.flatten(ps.map(p -> this.buildList(p.left, p.right)));
    }

    // Flatten a list of lists into a list
    // Eg. flatten([["a", "b"], ["c"], ["d", "e"]]) -> ["a", "b", "c", "d", "e"]
    private <X> Listof<X> flatten(Listof<Listof<X>> xss) {
        return xss.foldr(Listof<X>::append, new Empty<X>());
        // return xss.foldr((Listof<X> x, Listof<X> xs) -> x.append(xs), new Empty<X>());
    }

    // Hint: use foldr and append in Listof<X>:
    // <Y> Y foldr(BiFunction<X, Y, Y> f, Y b);
    // Listof<X> append(Listof<X> xs);

    // Another hint: in BSL
    // (define (flatten xss) (foldr xss append '()))

    // Given an element and nat. number, produce a list of n copies
    // Eg. buildList("a", 5) -> ["a", "a", "a", "a", "a"]
    private <X> Listof<X> buildList(X x, Integer n) {

        return n.equals(0) ? new Empty<>() :
                new Cons<>(x, this.buildList(x, n - 1));

        /*
        if (n.equals(0)) {
            return new Empty<X>();
        } else {
            return new Cons<X>(x, this.buildList(x, n - 1));
        }


        if (n > 0) {
            return new Cons<>(x, this.buildList(x, n - 1));
        } else {
            return new Empty<>();
        }

        if (n.equals(this.length())) {
            return new Empty<>();
        } else {
            return new Cons<>(x, this.buildList(x, n));
        }
        */

    }


}
