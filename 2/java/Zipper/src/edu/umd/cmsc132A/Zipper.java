package edu.umd.cmsc132A;

import java.util.Iterator;
import java.util.List;

class ListZipper<X> implements Iterator<X>, Iterable<X> {
    Listof<X> context;
    Listof<X> elems;

    ListZipper(Listof<X> elems) {
        this(AListof.empty(), elems);
    }

    ListZipper(Listof<X> context, Listof<X> elems) {
        this.context = context;
        this.elems = elems;
    }

    ListZipper<X> right() {
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

    ListZipper<X> left() {
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

    ListZipper<X> flip() {
        return new ListZipper<>(this.elems, this.context);
    }

    public String toString() {
        return "⟨" + context + ", " + elems + "⟩";
    }

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
}



class TreeZipper<X> {

    interface Pieces<X> {
        Tree<X> remake(Tree<X> focus);
    }

    class FillLeft<X> implements Pieces<X> {
        X value;
        Tree<X> right;

        FillLeft(X value, Tree<X> right) {
            this.value = value;
            this.right = right;
        }

        public Tree<X> remake(Tree<X> left) {
            return new Node(this.value, left, this.right);
        }
    }

    class FillRight<X> implements Pieces<X> {
        X value;
        Tree<X> left;

        FillRight(X value, Tree<X> left) {
            this.value = value;
            this.left = left;
        }

        public Tree<X> remake(Tree<X> right) {
            return new Node(this.value, this.left, right);
        }
    }

    Listof<Pieces<X>> context;
    Tree<X> elems;

    TreeZipper(Tree<X> elems) {
        this(AListof.empty(), elems);
    }

    TreeZipper(Listof<Pieces<X>> context, Tree<X> elems) {
        this.context = context;
        this.elems = elems;
    }

    TreeZipper<X> right() {
        return this.elems.accept(new TreeVisitor<X, TreeZipper<X>>() {
            public TreeZipper<X> visitLeaf(Leaf<X> leaf) {
                return TreeZipper.this;
            }
            public TreeZipper<X> visitNode(Node<X> node) {
                return new TreeZipper<>(
                        TreeZipper.this.context.cons(new FillRight<>(node.value, node.left)),
                        node.right
                );
            }
        });
    }

}



























































































/*
class ListZipper<X> implements Iterator<X>, Iterable<X> {

    // How to reconstruct the list
    Listof<X> context;
    Listof<X> list;

    ListZipper(Listof<X> l) {
        this(AListof.empty(), l);
    }

    ListZipper(Listof<X> context, Listof<X> list) {
        this.context = context;
        this.list = list;
    }

    ListZipper<X> right() {
        return this.list.accept(new ListVisitor<X, ListZipper<X>>() {
            public ListZipper<X> visitEmpty(Empty<X> mt) {
                return ListZipper.this;
            }
            public ListZipper<X> visitCons(Cons<X> cons) {
                return new ListZipper<>(new Cons<>(cons.first, ListZipper.this.context), cons.rest);
            }
        });
    }

    ListZipper<X> left() {
        return this.context.accept(new ListVisitor<>() {
            public ListZipper<X> visitEmpty(Empty<X> mt) {
                return ListZipper.this;
            }
            public ListZipper<X> visitCons(Cons<X> cons) {
                return new ListZipper<>(cons.rest, new Cons<>(cons.first, ListZipper.this.list));
            }
        });
    }

    ListZipper<X> flip() {
        return new ListZipper<>(this.list, this.context);
    }

    ListZipper<X> start() {
        return this.context.accept(new ListVisitor<>() {
            public ListZipper<X> visitEmpty(Empty<X> mt) {
                return ListZipper.this;
            }
            public ListZipper<X> visitCons(Cons<X> cons) {
                return ListZipper.this.left().start();
            }
        });
    }

    ListZipper<X> end() {
        return this.list.accept(new ListVisitor<>() {
            public ListZipper<X> visitEmpty(Empty<X> mt) {
                return ListZipper.this;
            }
            public ListZipper<X> visitCons(Cons<X> cons) {
                return ListZipper.this.right().end();
            }
        });
    }

    Listof<X> unzip() {
        return this.end().list;
    }

    public boolean hasNext() {
        return this.list.accept(new ListVisitor<X, Boolean>() {
            public Boolean visitEmpty(Empty<X> mt) {
                return false;
            }
            public Boolean visitCons(Cons<X> cons) {
                return true;
            }
        });
    }

    public X next() {
        return this.list.accept(new ListVisitor<X, X>() {
            public X visitEmpty(Empty<X> mt) {
                throw new RuntimeException("At the end of the list!");
            }
            public X visitCons(Cons<X> cons) {
                ListZipper<X> next = ListZipper.this.right();
                ListZipper.this.context = next.context;
                ListZipper.this.list = next.list;
                return cons.first;
            }
        });
    }

    public Iterator<X> iterator() {
        return this;
    }

    public String toString() {
        return "⟨" + context + ", " + list + "⟩";
    }
}

class TreeZipper<X> {

    interface Pieces<X> {
        Tree<X> remake(Tree<X> tree);
    }

    class Right<X> implements Pieces<X> {
        X value;
        Tree<X> right;
        Right(X value, Tree<X> right) {
            this.value = value;
            this.right = right;
        }
        public Tree<X> remake(Tree<X> left) {
            return new Node<X>(value, left, right);
        }
    }

    class Left<X> implements Pieces<X> {
        X value;
        Tree<X> left;
        Left(X value, Tree<X> left) {
            this.value = value;
            this.left = left;
        }
        public Tree<X> remake(Tree<X> right) {
            return new Node<X>(value, left, right);
        }
    }

    // How to reconstruct the tree
    Listof<Pieces<X>> context;
    Tree<X> current;

    TreeZipper(Tree<X> tree) {
        this(AListof.empty(), tree);
    }

    TreeZipper(Listof<Pieces<X>> context, Tree<X> tree) {
        this.context = context;
        this.current = tree;
    }

    TreeZipper<X> right() {
        return this.current.accept(new TreeVisitor<X, TreeZipper<X>>() {
            public TreeZipper<X> visitLeaf(Leaf<X> leaf) {
                return TreeZipper.this;
            }

            public TreeZipper<X> visitNode(Node<X> node) {
                return new TreeZipper<>(
                        context.cons(new Left<>(node.value, node.left)),
                        node.right
                );
            }
        });
    }

    TreeZipper<X> left() {
        return this.current.accept(new TreeVisitor<X, TreeZipper<X>>() {
            public TreeZipper<X> visitLeaf(Leaf<X> leaf) {
                return TreeZipper.this;
            }

            public TreeZipper<X> visitNode(Node<X> node) {
                return new TreeZipper<>(
                        context.cons(new Right<>(node.value, node.right)),
                        node.left
                );
            }
        });
    }

    TreeZipper<X> up() {
        return this.context.accept(new ListVisitor<Pieces<X>, TreeZipper<X>>() {
            public TreeZipper<X> visitEmpty(Empty<Pieces<X>> mt) {
                return TreeZipper.this;
            }

            public TreeZipper<X> visitCons(Cons<Pieces<X>> cons) {
                Pieces<X> pieces = cons.first;
                return new TreeZipper<>(
                        cons.rest,
                        pieces.remake(TreeZipper.this.current)
                );
            }
        });
    }
}
*/