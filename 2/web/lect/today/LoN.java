import tester.*;

interface Compare {
    Boolean compare(Integer i, Integer j);
}

class GreaterAbs implements Compare {
    GreaterAbs() {}
    public Boolean compare(Integer i, Integer j) {
	return (i*i) > (j*j);
    }
}

class GreaterThan implements Compare {
    GreaterThan () {}

    public Boolean compare(Integer i, Integer j) {
	return i > j;
    }
}

class LessThan implements Compare {
    LessThan () {}

    public Boolean compare(Integer i, Integer j) {
	return i < j;
    }
}

interface LoI {
    // Sort this list of integers (in given order)
    LoI sort(Compare c);

    // Insert given integer into this sorted list
    LoI insert(Integer i, Compare c);
}

class Empty implements LoI {
    Empty() {}

    public LoI sort(Compare c) {
	return this;
    }

    public LoI insert(Integer i, Compare c) {
	return new Cons(i, this);
    }
}

class Cons implements LoI {
    Integer first;
    LoI rest;
    Cons(Integer first, LoI rest) {
	this.first = first;
	this.rest = rest;
    }

    public LoI sort(Compare c) {
	return this.rest.sort(c).insert(this.first, c);
    }

    public LoI insert(Integer i, Compare c) {
	if (c.compare(i, this.first)) {
	    return new Cons(i, this);
	} else {
	    return new Cons(this.first, this.rest.insert(i, c));
	}
    }
}

class Examples {
    LoI abc = new Cons(1, new Cons(2, new Cons(3, new Empty())));
    LoI cba = new Cons(3, new Cons(2, new Cons(1, new Empty())));
    LoI def = new Cons(1, new Cons(-2, new Cons(3, new Empty())));
    void testSort(Tester t) {
	Compare lt = new LessThan();
	Compare gt = new GreaterThan();
	Compare absgt = new GreaterAbs();
	t.checkExpect(abc.sort(lt), abc);
	t.checkExpect(abc.sort(gt), cba);
	t.checkExpect(abc.sort(absgt), cba);
	t.checkExpect(def.sort(absgt),
		      new Cons(3, new Cons(-2, new Cons(1, new Empty()))));
    }
}