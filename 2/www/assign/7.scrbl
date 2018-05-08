#lang scribble/manual
@(require scribble/core)
@(require "../utils.rkt")
@(define (colorize c . content)
  (elem #:style (style #f (list (color-property c)))
        content))

@assn-title[7]{Traversing Trees}

This is assignment is to be completed and submitted with your new
@link["https://piazza.com/class/jcspfhewmdn41y?cid=108"]{partner}.  You
may not work with anyone other than your assigned partner.

@bold{Due}: Wednesday, May 9, 11:59:59 PM EST.

@(define @Piazza @link["http://piazza.com/umd/spring2018/cmsc132a"]{Piazza})

@section{Cleaned-up Traversals and Iterators over Trees}

For this assignment, you will start from a cleaned-up version
(@link["Assign7.zip"]{Assign7.zip}) of the pre-order binary tree
traversal code that we wrote in class.

The main idea in this code is to implement an @tt{Iterator} for trees
that delivers elements of a tree according to a pre-order (NLR)
traversal.

To tackle this problem, we developed a (functional) interface for
traversing binary trees (this is slightly tweaked from what we saw in
class):

@verbatim|{
interface TreeTraversal<X> {
    // Advance the traversal (if possible)
    TreeTraversal<X> next();

    // Get the current element in this traversal (if there is one)
    X get();

    // Is there a current element in this traversal?
    Boolean has();
}
}|

To implement a @tt{TreeTraversal} we fundamentally need two kinds of
traversals: one for traversing an empty tree (a leaf) and one for
traversing a node.

The leaf iterator is totally easy:
@verbatim|{
class LeafTraversal<X> implements TreeTraversal<X> {
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
}|


To node iterator is more involved, but here we can offload all of the
hard work to a @tt{TreeVisitor} that computes the next state of the
traversal based on the current node:
@verbatim|{
class NLR<X> implements TreeTraversal<X> {
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
}|

Now the whole problem has boiled down to the @tt{NLRNext} visitor,
which visits a node and produces the next @tt{TreeTraversal} based on
that node and the current context.

Computing what the next @tt{TreeTraversal} should be boils down to a
case analysis on both children of the node.  Here is the complete set of cases to 
consider:

@itemlist[
@item{The left and right are leafs.

Since both are leafs, we must look to the context for potentially
supplying the next node to visit.  If there are no nodes there (the
list is empty), the next traversal is just a @tt{LeafTraversal}.
Otherwise, the first element becomes the new current node and is
removed from the context.}

@item{The left is a leaf, the right is a node.

The right becomes the new current node; the context remains unchanged.
}

@item{The left is a node, the right is a leaf.

The left becomes the new current ndoe; the context remains unchanged.}

@item{The left is a node, the right is a node.

The left becomes the new current node; the right is added to the context.}
]

In order to write code that does this kind of case analysis on the
children of a node bearable to read and maintain, I made an interface
for such computations:

@verbatim|{
// Interface for any computation over a Node
// that needs to do case analysis on both subtrees
interface NodeChildVisitor<X, R> {
    // Both are leafs
    R visitLeafLeaf();

    // Left is a leaf, right is node
    R visitLeafNode(Node<X> right);

    // Left is a node, right is a leaf
    R visitNodeLeaf(Node<X> left);

    // Both are nodes
    R visitNodeNode(Node<X> left, Node<X> right);
}
}|

We can translate the above case analysis into an implementation:

@verbatim|{
// Visit a node to advance to the next traversal
class NLRNext<X> implements NodeChildVisitor<X, TreeTraversal<X>> {
    Listof<Node<X>> context;

    NLRNext(Listof<Node<X>> context) {
        this.context = context;
    }

    // Both are leafs:
    // look to the context
    public TreeTraversal<X> visitLeafLeaf() {
        return this.context.accept(new ListVisitor<Node<X>, TreeTraversal<X>>() {
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
}|

At this point, we've done the hard stuff, but some plumbing remains.
In particular, we have a @tt{NodeChildVisitor} for computing the next
traversal, but what we actually need is a @tt{TreeVisitor} (go back
and look at the @tt{next} method in @tt{NLR}).  But a
@tt{NodeChildVisitor} can be transformed to behave as thought it is a
@tt{TreeVisitor}.  To do this, we make the following changes:

@itemlist[

@item{Add an @tt{extends TreeVisitor<X, R>} clause to @tt{NodeChildVisitor}.}

@item{Define an abstract class @tt{ANodeChildVisitor<X, R>} that
implements @tt{NodeChildVisitor<X, R>}.}

@item{Add an @tt{extends ANodeChildVisitor<X, TreeTraversal<X>>}
clause to @tt{NLRNext}}

@item{Implement @tt{visitLeaf} and @tt{visitNode} methods in the
@tt{ANodeChildVisitor} abstract class.}
]

This set-up has the nice property that any @tt{NodeChildVisitor} can
be used as a @tt{TreeVisitor}, and that awful nesting of anonymous
inner-classes that we wrote in lecture in order to the case analysis
on children of a node can be written generically and just once.

You can read the @tt{ANodeChildVisitor} code if you'd like, but you
won't need to change it or write code like it.  If you need to do case
analysis on the children of a node, simply extend the class and
implement the four methods of the @tt{NodeChildVisitor} interface like
we did for @tt{NLRNext}.

Be sure to look at the test suite for examples of how all this stuff
works.

@section{Traversing In-order (LNR)}

Following the pattern of @tt{MakeNLR<X>}, which implements
@tt{TreeVisitor<X, TreeTraversal<X>>}, design a class @tt{MakeLNR<X>}
that implements @tt{TreeVisitor<X, TreeTraversal<X>>}.  It's purpose
is to visit a tree and construct an @emph{in-order} traversal of the
elements of the tree.

In-order means that for a node, all the elements of the left subtree
are traversed, then the element, the all the elements of the right.
(Unlike NLR, which went node, left, right.)

For full credit, each of the operations (including the construction of
the traversal) should have a cost bounded by the height of the tree.
For partial credit, implement the traversal but ignore this
requirement.

The change compared to NLR will be in the representation of the
context and writing a @tt{LNRNext} visitor.

Hint: make examples on penci and paper to help guide you through what
information will need to be represented.  Spending a bit of time doing
this will save you loads of time coding.

@section{Traversing Post-order (LRN)}

Following the pattern of @tt{MakeNLR<X>}, which implements
@tt{TreeVisitor<X, TreeTraversal<X>>}, design a class @tt{MakeLRN<X>}
that implements @tt{TreeVisitor<X, TreeTraversal<X>>}.  It's purpose
is to visit a tree and construct an @emph{post-order} traversal of the
elements of the tree.

Post-order means that for a node, all the elements of the left subtree
are traversed, all the all the elements of the right are traversed,
then the element.  (Unlike NLR, which went node, left, right.)

For full credit, each of the operations (including the construction of
the traversal) should have a cost bounded by the height
of the tree.  For partial credit, implement the traversal but ignore
this requirement.

The change compared to NLR will be in the representation of the
context and writing a @tt{LRNNext} visitor.

Hint: make examples on pencil and paper to help guide you through what
information will need to be represented.  Spending a bit of time doing
this will save you loads of time coding.



@section[#:style 'unnumbered #:tag "assign6:submit"]{Submission}

Use @tt{submit.cs.umd.edu} to submit your solution to the problems.
You should create a zip file called @tt{Assign6.zip} that contains the
IntelliJ project containing your solutions.
