interface BT<X> {
    <R> R accept(BTVisitor<X,R> v);
}

interface BTVisitor<X,R> {
    R visitLeaf();
    R visitNode(X val, BT<X> left, BT<X> right);
}

class MaybeCons<X> implements BTVisitor<X,List<Node<X>>> {
    List<Node<X>> ls;
    MaybeCons(List<Node<X>> ls) {
	this.ls = ls;
    }
    public List<Node<X>> visitLeaf() { return this.ls; }
    public List<Node<X>> visitNode(X val, BT<X> left, BT<X> right) {
	return new Cons<Node<X>>(new Node<X>(val, left, right));
    }
}

class Leaf<X> implements BT<X> {
    public <R> R accept(BTVisitor<X,R> v) {
	return v.visitLeaf();
    }
}

class Node<X> implements BT<X> {
    X val;
    BT<X> left;
    BT<X> right;
    Node(X val, BT<X> left, BT<X> right) {
	this.val = val;
	this.left = left;
	this.right = right;
    }
    public <R> R accept(BTVisitor<X,R> v) {
	return v.visitNode(this.val, this.left, this.right);
    }
}

class BTIterator<X> implements Iterator<X> {
    List<Node<X>> ls;
    BTIterator(Node<X> bt) {
	this.ls = new Cons<Node<X>>(bt, new Empty<Node<X>>());
    }
    public X next() {
	X first = ls.accept(new First<X>());
	BTVisitor<X,List<Node<X>>> v1 = new MaybeCons(ls.accept(new Rest<X>()));
	BTVisitor<X,List<Node<X>>> v2 = new MaybeCons(v1);
	ls = ls.accept(v2);
	return first;
    }
    public boolean hasNext() {
	for (Node<X> n : ls) return true;
	return false;
    }
    public void remove() {}
}

interface ListVisitor<X,R> {
    R visitEmpty();
    R visitCons(X first, List<X> rest);
}

class First<X> implements ListVisitor<X,X> {
    public X visitEmpty() { throw new RuntimeException("Undefined"); }
    public X visitCons(X first, List<X> rest) { return first; }
}

class Rest<X> implements ListVisitor<X,List<X>> {
    public List<X> visitEmpty() { throw new RuntimeException("Undefined"); }
    public List<X> visitCons(X first, List<X> rest) { return rest; }
}

interface List<X> extends Iterable<X> {
    // add more list stuff...
    Boolean isEmpty();
    X getFirst();
    List<X> getRest();
    <R> R accept(ListVisitor<X,R> v);
}

abstract class AList<X> implements List<X> {
    public Iterator<X> iterator() {
	return new ListIterator<X>(this);
    }
}

class Empty<X> extends AList<X> {
    Empty() {}
    public Boolean isEmpty() { return true; }
    public X getFirst() { throw new RuntimeException("Empty list"); }
    public List<X> getRest() { throw new RuntimeException("Empty list"); }
    public <R> R accept(ListVisitor<X,R> v) { return v.visitEmpty(); }
}

class Cons<X> extends AList<X> {
    X first;
    List<X> rest;
    Cons(X first, List<X> rest) {
	this.first = first;
	this.rest = rest;
    }
    public Boolean isEmpty() { return false; }
    public X getFirst() { return this.first; }
    public List<X> getRest() { return this.rest; }

    public <R> R accept(ListVisitor<X,R> v) {
	return v.visitCons(this.first, this.rest);
    }
}

// Represents an iteration through a list.
class ListIterator<X> implements Iterator<X> {
    List<X> list;
    ListIterator(List<X> list) {
	this.list = list;
    }

    public boolean hasNext() {
	return !(this.list.isEmpty());
    }

    public X next() {
	X res = this.list.getFirst();
	this.list = this.list.getRest();
	return res;
    }

    public void remove() {}
}
