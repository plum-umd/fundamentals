#lang scribble/manual
@(require scribble/eval
          racket/sandbox
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ... check-expect))
          (for-label (except-in class/0 define-struct ... check-expect))
          (for-label class/universe)
          "../utils.rkt")

@lecture-title[36]{Zippers}

@section[#:style 'unnumbered #:tag "lec36:alt"]{Alternating iterator}

At the end of the previous lecture we were asked to consider an
alternating iterator: Given two iterators, construct an iterator that
will alternate between taking an element from each of the two. For
example:

@verbatim|{
Iterator<Integer> l123 = AListof.make(3, n -> n+1).iterator();
Iterator<Integer> l456 = AListof.make(3, n -> n+4).iterator();
Iterable<Integer> alt = new Alt<>(l123, l456);
for (Integer i : alt) {
  System.out.println(i);
}
// Should print 1 4 2 5 3 6
}|

The outline of the Java iterator looks like this:

@verbatim|{
public class Alt<X> implements Iterator<X>, Iterable<X> {
    Iterator<X> first;
    Iterator<X> second;

    Alt(Iterator<X> first, Iterator<X> second) {
        this.first = first;
        this.second = second;
    }

    public boolean hasNext() {
        return false;
    }

    public X next() {
        return null;
    }
}
}|

The first suggested solution involved a flag that indicates which
iterator to get the next element from. Since there are two possible
iterators, a Boolean field makes a suitable flag. 

@verbatim|{
public class Alt<X> implements Iterator<X>, Iterable<X> {
    Iterator<X> first;
    Iterator<X> second;
    Boolean flag; // true => first, false => second

    Alt(Iterator<X> first, Iterator<X> second) {
        this.first = first;
        this.second = second;
        this.flag = true;
    }

    public boolean hasNext() {
        return this.flag ? this.first.hasNext() : this.second.hasNext();
    }

    public X next() {
        X tmp = this.flag ? this.first.next() : this.second.next();
        this.flag = !this.flag;
        return tmp;
    }
}
}|

There is another possible solution, and one which does not require any
additional fields to be defined. By swapping the @tt{first} and
@tt{second} iterators each time @tt{next} is called, we can
unconditionally pull elements out of the first iterator.

@verbatim|{
public class Alt<X> implements Iterator<X>, Iterable<X> {
    Iterator<X> first;
    Iterator<X> second;

    Alt(Iterator<X> first, Iterator<X> second) {
        this.first = first;
        this.second = second;
    }

    public boolean hasNext() {
        return this.first.hasNext();
    }

    public X next() {
        X next = this.first.next();
        Iterator<X> tmp = this.first;
        this.first = this.second;
        this.second = tmp;
        return next;
    }
}
}|


@section[#:style 'unnumbered #:tag "lec36:uhoh"]{Reusing an iterator}

With either implementation from the last section, our alternating
iterator example functions as expected:

@verbatim|{
Iterator<Integer> l123 = AListof.make(3, n -> n+1).iterator();
Iterator<Integer> l456 = AListof.make(3, n -> n+4).iterator();
Iterable<Integer> alt = new Alt<>(l123, l456);
for (Integer i : alt) {
  System.out.println(i);
}
// Prints 1 4 2 5 3 6
}|

But what happens if we try to get the next value out of the iterator
@tt{l123}, which used to contain the integers 1, 2, and 3?

@verbatim|{
System.out.println(l123.next());  // => Error: No more elements!
}|

Despite not being touched in our example code, behind the scenes the
elements of the iterator @tt{l123} were exhausted. Looking at the
@tt{ListIterator.next()} method, it's obvious why this happened:

@verbatim|{
// EFFECT: Updates iterator to point to next element;
// EFFECT: Throws runtime exception if `elems' is empty;
// Returns the next element of this list.
public X next() {
    return this.elems.accept(new ListVisitor<>() {
        public X visitEmpty(Empty<X> mt) {
            throw new RuntimeException("No more elements!");
        }
        public X visitCons(Cons<X> cons) {
            ListIterator.this.elems = cons.rest;
            return cons.first;
        }
    });
}
}|

The iterator branches on the underlying list. When the list is
non-empty, the field holding the list is mutated to point to the rest
of that list, and the first of the list is returned. Once that element
is removed, the iterator forgets about it and is unable to rebuild the
original list.

What information do we need to save to rebuild the original list?


@section[#:style 'unnumbered #:tag "lec36:zipper"]{Iterator + Context = Zipper}

As we noted, each time we call the @tt{ListIterator.next()} method we
drop that element on the floor. Instead, let's hold on to all the
elements we've seen as we zip our way through the list.

We'll start a new class definition based on our @tt{ListIterator}
called the @tt{ListZipper}. This definition has an additional field:
the @tt{context}. As we move our way through the list, we'll
accumulate the elements we pass in the @tt{context} of the
zipper. When the zipper is created with some elements, we begin at the
head of that list with an empty context.

@verbatim|{
class ListZipper<X> {
    private Listof<X> context;
    private Listof<X> elems;

    ListZipper(Listof<X> elems) {
        this.context = AListof.empty();
        this.elems = elems;
    }

    // Creates a string representation of this zipper
    public String toString() {
        return "⟨" + context + ", " + elems + "⟩";
    }
}
}|

Similar to the @tt{ListIterator.next()} method, we can move to the
@tt{right} inside the @tt{ListZipper} by branching on the elements. We
create a new zipper with current context extended with the head of the
elements and the rest of the elements.

@verbatim|{
// Move one element to the right from this zipper;
// If no more elements remain, return this zipper
public ListZipper<X> right() {
    return this.elems.accept(new ListVisitor<X, ListZipper<X>>() {
        public ListZipper<X> visitEmpty(Empty<X> mt) {
            return ListZipper.this;
        }
        public ListZipper<X> visitCons(Cons<X> cons) {
            return new ListZipper<>(
                    ListZipper.this.context.cons(cons.first),
                    cons.rest
            );
        }
    });
}
}|

A small example will illustrate how the @tt{right} operation works:

@verbatim|{
ListZipper<Integer> zip = new ListZipper<>(AListof.make(3, n -> n+1));
System.out.println(zip);                          // => ⟨[], [1, 2, 3]⟩
System.out.println(zip.right());                  // => ⟨[1], [2, 3]⟩
System.out.println(zip.right().right());          // => ⟨[2, 1], [3]⟩
System.out.println(zip.right().right().right());  // => ⟨[3, 2, 1], []⟩
}|

As we move our way to the right in the @tt{zip}, the elements of the
original list accumulate in reverse in the context. Note that the most
recently seen element of the list is placed at the head of the
context.

With the @tt{right} method implemented, we can make the
@tt{ListZipper} an @tt{Iterable<X>} @tt{Iterator<X>} again:

@verbatim|{
public Iterator<X> iterator() {
        return this;
}

public boolean hasNext() {
    return this.elems.accept(new ListVisitor<X, Boolean>() {
        public Boolean visitEmpty(Empty<X> mt) {
            return false;
        }
        public Boolean visitCons(Cons<X> cons) {
            return true;
        }
    });
}

public X next() {
    return this.elems.accept(new ListVisitor<X, X>() {
        public X visitEmpty(Empty<X> mt) {
            throw new RuntimeException("No more elements!");
        }
        public X visitCons(Cons<X> cons) {
            ListZipper<X> next = ListZipper.this.right();
            ListZipper.this.context = next.context;
            ListZipper.this.elems = next.elems;
            return cons.first;
        }
    });
}
}|

But what have we gained? Our motivating example still doesn't work!

@verbatim|{
ListZipper<Integer> zip = new ListZipper<>(AListof.make(3, n -> n+1));
for (Integer i : zip) {
  System.out.println(i);
}
// Prints 1 2 3

System.out.println(zip.next());  // => Error: No more elements!
}|

Unlike the original iterator, We can recreate the original list by
moving to the @tt{left} inside the zipper.

@verbatim|{
// Move one element to the left from this zipper;
// If no more elements remain, return this zipper
public ListZipper<X> left() {
    return this.context.accept(new ListVisitor<X, ListZipper<X>>() {
        public ListZipper<X> visitEmpty(Empty<X> mt) {
            return ListZipper.this;
        }
        public ListZipper<X> visitCons(Cons<X> cons) {
            return new ListZipper<>(
                    cons.rest,
                    ListZipper.this.elems.cons(cons.first)
            );
        }
    });
}
}|

The @tt{left} method undoes a step to the @tt{right}. We look at the
context, and if it is non-empty, shift the most recent element from
the context (its head) back to the elements of the zipper.

@verbatim|{
ListZipper<Integer> zip = new ListZipper<>(AListof.make(3, n -> n+1));
for (Integer i : zip) {
  System.out.println(i);
}
// Prints 1 2 3

System.out.println(zip.left().next());  // => 3

for (Integer i : zip.left().left().left()) {
  System.out.println(i);
}
// Prints 1 2 3
}|

When we take a step to the left in our example, we get to print the
element 3 for a second time! If we take three steps to the left, we
can print all the elements again!

Of course it's a pain to step to the left three times, when we can
just go to the @tt{start}:

@verbatim|{
ListZipper<X> start() {
    return this.context.accept(new ListVisitor<X, ListZipper<X>>() {
        public ListZipper<X> visitEmpty(Empty<X> mt) {
            return ListZipper.this;
        }
        public ListZipper<X> visitCons(Cons<X> cons) {
            return ListZipper.this.left().start();
        }
    });
}
}|

If the context is non-empty, we step to the left and recur. If the
context is empty, we've fully rewound any iteration we've performed
and return this zipper.

Similarly, we can easily get to the end of the list:

@verbatim|{
ListZipper<X> end() {
    return this.elems.accept(new ListVisitor<X, ListZipper<X>>() {
        public ListZipper<X> visitEmpty(Empty<X> mt) {
            return ListZipper.this;
        }
        public ListZipper<X> visitCons(Cons<X> cons) {
            return ListZipper.this.right().end();
        }
    });
}
}|

The @tt{ListZipper} is similar to a data structure we implemented in
homework and labs last semester: a text-box. The cursor falls between
two strings and can be shifted left and right among the
characters.

The @tt{ListZipper} represents a position inside of a list. We can
change position by shifting elements to the right or left of the
current focus. Crucially, we always hold on to the full context: both
the elements behind and in front of the current focus.

The notion of a zipper does not only apply to lists. We can move
through any data structure in a similar way, as long as we never throw
any information away.


@section[#:style 'unnumbered #:tag "lec36:tree"]{Climbing a Tree}

The following is a simple binary tree implementation:

@verbatim|{
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
}|

We could move to the left or right inside of a list using the
@tt{ListZipper}. In a binary tree we have three directions that we can
travel: down to the left, down to the right, or up the tree.

When we move to the right inside a list, we needed to hold on to
@tt{first} element in our context in order to move back to the
left. If we focus on the right branch of some node, to move back up we
must hold on to both the value of that node and its left branch: both
the pieces necessary to reconstruct the original node.

We can represent each piece of the context of a tree as node with a
hole on the left or a hole on the right. The key functionality the
context must provide is a method to @tt{remake} the original tree by
plugging the hole.

@verbatim|{
interface NodeWithHole<X> {
    Tree<X> remake(Tree<X> focus);
}

class HoleOnLeft<X> implements NodeWithHole<X> {
    X value;
    Tree<X> right;

    HoleOnLeft(X value, Tree<X> right) {
        this.value = value;
        this.right = right;
    }

    public Tree<X> remake(Tree<X> left) {
        return new Node(this.value, left, this.right);
    }
}

class HoleOnRight<X> implements NodeWithHole<X> {
    X value;
    Tree<X> left;

    HoleOnRight(X value, Tree<X> left) {
        this.value = value;
        this.left = left;
    }

    public Tree<X> remake(Tree<X> right) {
        return new Node(this.value, this.left, right);
    }
}
}|

With these class definitions, we can begin to implement our
@tt{TreeZipper}.

@verbatim|{
class TreeZipper<X> {
    Listof<NodeWithHole<X>> context;
    Tree<X> elems;

    TreeZipper(Tree<X> elems) {
        this(AListof.empty(), elems);
    }

    TreeZipper(Listof<Pieces<X>> context, Tree<X> elems) {
        this.context = context;
        this.elems = elems;
    }
}|

Our context is a list of holey nodes. Each time we step down to the
left or right inside a node, we place the other pieces into the
context.

Moving to the right, we place the left sub-tree and the node's value
into the context:

@verbatim|{
TreeZipper<X> right() {
    return this.elems.accept(new TreeVisitor<>() {
        public TreeZipper<X> visitLeaf(Leaf<X> leaf) {
            return TreeZipper.this;
        }
        public TreeZipper<X> visitNode(Node<X> node) {
            return new TreeZipper<>(
                    TreeZipper.this.context.cons(new HoleOnRight<>(node.value, node.left)),
                    node.right
            );
        }
    });
}
}|

Moving to the left, we place the right sub-tree and the node's value
into the context:

@verbatim|{
TreeZipper<X> left() {
    return this.elems.accept(new TreeVisitor<>() {
        public TreeZipper<X> visitLeaf(Leaf<X> leaf) {
            return TreeZipper.this;
        }
        public TreeZipper<X> visitNode(Node<X> node) {
            return new TreeZipper<>(
                    TreeZipper.this.context.cons(new HoleOnLeft<>(node.value, node.right)),
                    node.left
            );
        }
    });
}
}|

To move back up to the parent node, we simply @tt{remake} the previous
node from the context:

@verbatim|{
TreeZipper<X> up() {
    return this.context.accept(new ListVisitor<>() {
        public TreeZipper<X> visitEmpty(Empty<NodeWithHole<X>> mt) {
            return TreeZipper.this;
        }
        public TreeZipper<X> visitCons(Cons<NodeWithHole<X>> cons) {
            return new TreeZipper<>(
                    cons.rest,
                    cons.first.remake(this.elems)
            );
        }
    });
}
}|
