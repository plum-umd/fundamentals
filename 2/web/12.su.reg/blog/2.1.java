import tester.*;

// Represents a list of integers
interface LoI {
    // Sum this list of integers.
    Integer sum();

    // Sum the squares of this list of integers.
    Integer sumSquares();
}

// Represents an empty list of integers
class Empty implements LoI {
    Empty() {}

    // Sum this empty list of integers
    public Integer sum() {
	return 0;
    }

    // Sum the squares of this empty list of integers.
    public Integer sumSquares() {
	return 0;
    }
}

class Cons implements LoI {
    Integer first;
    LoI rest;
    Cons(Integer first, LoI rest) {
	this.first = first;
	this.rest = rest;
    }

    // Sum this non-empty list of integers.
    public Integer sum() {
	return this.first + this.rest.sum();
    }

    // Sum the squares of this non-empty list of integers.
    public Integer sumSquares() {
	return (this.first * this.first) + this.rest.sumSquares();
    }
}

class Examples {
    Examples() {}

    LoI mt = new Empty();
    LoI l1 = new Cons(1, mt);
    LoI l2 = new Cons(2, l1);
    LoI l3 = new Cons(3, l2);

    void testSum(Tester t) {
	t.checkExpect(mt.sum(), 0);
	t.checkExpect(l1.sum(), 1);
	t.checkExpect(l3.sum(), 6);
    }

    void testSumSquares(Tester t) {
	t.checkExpect(mt.sumSquares(), 0);
	t.checkExpect(l1.sumSquares(), 1);
	t.checkExpect(l3.sumSquares(), 14);
    }
}