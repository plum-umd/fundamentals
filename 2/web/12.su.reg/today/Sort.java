import tester.*;

// Represents a list of integers
interface LoI {
    // Sort this list of integers in ascending order.
    LoI sort();

    // Insert the given integer into this sorted list of integers,
    // to produce a sorted list in ascending order.
    LoI insert(Integer i);

    // Reverse the elements of this list.
    LoI reverse();

    // Put the given element at the end of this list.
    LoI putAtEnd(Integer i);
}

// Represents an empty list of integers
class Empty implements LoI {
    Empty() {}

    // Sort this empty list of integers in ascending order.
    public LoI sort() {
	return this;
    }

    public LoI insert(Integer i) {
	return new Cons(i, this);
    }

    // Reverse the elements of this empty list.
    public LoI reverse() {
	return this;
    }

    // Put the given element at the end of this empty list.
    public LoI putAtEnd(Integer i) {
	return new Cons(i, this);
    }
}

// Represents an non-empty list of integers
class Cons implements LoI {
    Integer first;
    LoI rest;
    Cons(Integer first, LoI rest) {
	this.first = first;
	this.rest = rest;
    }

    // Sort this non-empty list of integers in ascending order.
    public LoI sort() {
	return this.rest.sort().insert(this.first);
    }

    public LoI insert(Integer i) {
	if (i > this.first) {
	    return new Cons(this.first,
			    this.rest.insert(i));
	} else {
	    return new Cons(i, this);
	}
    }

    // Reverse the elements of this non-empty list.
    public LoI reverse() {
	return this.rest.reverse().putAtEnd(this.first);
    }

    // Put the given element at the end of this non-empty list.
    public LoI putAtEnd(Integer i) {
	return new Cons(this.first,
			this.rest.putAtEnd(i));
    }
}

class Examples {
    Examples() {}

    LoI mt = new Empty();
    LoI one = new Cons(1, mt);
    LoI l1 = new Cons(4, new Cons(3, new Cons(1, mt)));
    LoI l2 = new Cons(2, new Cons(3, new Cons(1, mt)));
    LoI sl1 = new Cons(1, new Cons(3, new Cons(4, mt)));
    LoI sl2 = new Cons(1, new Cons(4, mt));

    void testReverse(Tester t) {
	t.checkExpect(mt.reverse(), mt);
	t.checkExpect(one.reverse(), one);
	t.checkExpect(sl2.reverse(), new Cons(4, new Cons(1, mt)));
    }

    void testInsert(Tester t) {
	t.checkExpect(mt.insert(1), one);
	t.checkExpect(sl2.insert(3), sl1);
    }

    void testSort(Tester t) {
	t.checkExpect(mt.sort(), new Empty());
	t.checkExpect(one.sort(), one);
	t.checkExpect(sl1.sort(), sl1);
	t.checkExpect(l1.sort(),
		      new Cons(1, new Cons(3, new Cons(4, mt))));
    }
}
