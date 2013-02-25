import tester.*;

// Represents a computation over a list of integers producing a Result
interface ListVisitor<Result> {
    Result visitEmpty();
    Result visitCons(Integer first, ListInteger rest);
}

class Equal implements ListVisitor<Boolean> {
    ListInteger other;
    Equal(ListInteger other) {
	this.other = other;
    }
    public Boolean visitEmpty() {
	return other.accept(new IsEmpty());
    }
    public Boolean visitCons(Integer first, ListInteger rest) {
	return other.accept(new SameCons(first, rest));
    }
}

class IsEmpty implements ListVisitor<Boolean> {
    IsEmpty() {}
    public Boolean visitEmpty() { return true; }
    public Boolean visitCons(Integer first, ListInteger rest) { return false; }
}

class SameCons implements ListVisitor<Boolean> {
    Integer first;
    ListInteger rest;
    SameCons(Integer first, ListInteger rest) {
	this.first = first;
	this.rest = rest;
    }
    public Boolean visitEmpty() { return false; }
    public Boolean visitCons(Integer first, ListInteger rest) {
	return this.first == first
	    && this.rest.accept(new Equal(rest));
    }
}


// Represents the sum computation over lists of integers.
class Sum implements ListVisitor<Integer> {
    Sum() {}
    public Integer visitEmpty() {
	return 0;
    }
    public Integer visitCons(Integer first, ListInteger rest) {
	return first + rest.accept(this);
    }
}

interface ListInteger {
    // Compute the largest element in a non-empty list.
    Integer max();

    // Compute the largest number in this list and acc.
    // ACC: the largest number seen so far.
    Integer maxAcc(Integer acc);

    // Accept the given visitor and compute a result.
    <R> R accept(ListVisitor<R> v);
}

class Empty implements ListInteger {
    Empty() {}

    // Compute the largest element in a non-empty list.
    public Integer max() {
	throw new RuntimeException("Can't take the max of an empty list, mom.");
    }

    // Compute the largest element in this list and acc, or acc if no elements.
    // ACC: the largest number seen so far.
    public Integer maxAcc(Integer acc) {
	return acc;
    }

    public <R> R accept(ListVisitor<R> v) {
	return v.visitEmpty();
    }
}

class Cons implements ListInteger {
    Integer first;
    ListInteger rest;
    Cons(Integer first, ListInteger rest) {
	this.first = first;
	this.rest = rest;
    }

    // Compute the largest element in a non-empty list.
    public Integer max() {
	return this.rest.maxAcc(this.first);
    }

    // Compute the largest element in this list and acc, or acc if no elements.
    // ACC: the largest number seen so far.
    public Integer maxAcc(Integer acc) {
	if (acc > this.first) {
	    return this.rest.maxAcc(acc);
	} else {
	    return this.rest.maxAcc(this.first);
	}
    }

    public <R> R accept(ListVisitor<R> v) {
	return v.visitCons(this.first, this.rest);
    }
}

class Examples {
    Examples() {}

    void testMax(Tester t) {
	// t.checkExpect(new Empty().max(), new RuntimeException("Boo"));
    }

    void testMaxAcc(Tester t) {
	t.checkExpect(new Empty().maxAcc(42), 42);
	t.checkExpect(new Cons(43, new Empty()).maxAcc(42), 43);
	t.checkExpect(new Cons(41, new Empty()).maxAcc(42), 42);
    }

    void testSum(Tester t) {
	t.checkExpect(new Empty().accept(new Sum()), 0);
	t.checkExpect(new Cons(42, new Cons(2, new Empty())).accept(new Sum()),
		      44);
    }

    void testEqual(Tester t) {
	ListInteger l1 = new Cons(42, new Cons(2, new Empty()));
	ListInteger l2 = new Cons(42, new Empty());
	t.checkExpect(l1.accept(new Equal(l1)), true);
	t.checkExpect(l2.accept(new Equal(l2)), true);
	t.checkExpect(l1.accept(new Equal(l2)), false);
	t.checkExpect(l2.accept(new Equal(l1)), false);
    }
}

