import tester.*;

interface List<X> {
    void setTail(List<X> ls);
    void setTailAcc(List<X> ls, Cons<X> last);
}

class MT<X> implements List<X> {
    public void setTail(List<X> ls) {
	throw new RuntimeException("setTail on MT");
    }
    public void setTailAcc(List<X> ls, Cons<X> last) {
	last.rest = ls;
    }
}

class Cons<X> implements List<X> {
    X first;
    List<X> rest;
    Cons(X first, List<X> rest) {
	this.first = first;
	this.rest = rest;
    }
    public void setTail(List<X> ls) {
	this.rest.setTailAcc(ls, this);
    }
    public void setTailAcc(List<X> ls, Cons<X> last) {
	this.rest.setTailAcc(ls, this);
    }
}

class Examples {
    void testSetTail(Tester t) {
	List<Integer> one = new Cons<Integer>(1, new MT<Integer>());
	List<Integer> two = new Cons<Integer>(2, new MT<Integer>());
	one.setTail(two);
	t.checkExpect(one, new Cons<Integer>(1,
					     new Cons<Integer>(2,
							       new MT<Integer>())));
	t.checkExpect(((Cons)one).rest == two, true);
    }
    void testSetTailCycle(Tester t) {
	List<Integer> ones = new Cons<Integer>(1, new MT<Integer>());
	ones.setTail(ones);
	t.checkExpect(((Cons)ones).first, 1);
	t.checkExpect(((Cons)((Cons)ones).rest).first, 1);
	t.checkExpect(((Cons)((Cons)((Cons)ones).rest).rest).first, 1);
	t.checkExpect(((Cons)ones).rest == ones, true);
    }
}
