import tester.*;

interface Pred<X> extends Fun<X,Boolean> {
    // Does this predicate hold for x?
    // Boolean apply(X x);
}

class IsEven implements Pred<Integer> {
    // Is x even?
    public Boolean apply(Integer x) {
	return (x % 2) == 0;
    }
}

class IsWilma implements Pred<String> {
    // Is x the string "Wilma"?
    public Boolean apply(String x) {
	return x.equals("Wilma");
    }
}

interface Fun<X,Y> {
    // Apply this function to x.
    Y apply(X x);
}

interface Lo<Something> {
    Lo<Something> filter(Pred<Something> p);

    <SomethingElse> Lo<SomethingElse> map(Fun<Something,SomethingElse> f);
}

class Empty<Something> implements Lo<Something> {
    Empty() {}

    public Lo<Something> filter(Pred<Something> p) {
	return this;
    }

    public <SomethingElse> Lo<SomethingElse> map(Fun<Something,SomethingElse> f) {
	return new Empty<SomethingElse>();
    }
}

class Cons<X> implements Lo<X> {
    X first;
    Lo<X> rest;
    Cons(X first, Lo<X> rest) {
	this.first = first;
	this.rest = rest;
    }

    // Produced the list of elements in this list that satisify p.
    public Lo<X> filter(Pred<X> p) {
	if (p.apply(this.first)) {
	    return new Cons<X>(this.first, this.rest.filter(p));
	} else {
	    return this.rest.filter(p);
	}
    }

    public <SomethingElse> Lo<SomethingElse> map(Fun<X,SomethingElse> f) {
	return new Cons<SomethingElse>(f.apply(this.first), this.rest.map(f));
    }
}

class ToString implements Fun<Integer,String> {

    // Converts a number into a string.
    public String apply(Integer x) {
	return x.toString();
    }
}

class Examples {
    Lo<String> emptystrings = new Empty<String>();
    Lo<String> los = new Cons<String>("hello", emptystrings);
    Lo<Integer> emptyints = new Empty<Integer>();
    Lo<Integer> loi = new Cons<Integer>(5, emptyints);
    Lo<Lo<Integer>> loloi = new Cons<Lo<Integer>>(loi, new Empty<Lo<Integer>>());

    void testMap(Tester t) {
	Fun<Integer,String> toString = new ToString();
	t.checkExpect(loi.map(toString),
		      new Cons<String>("5", new Empty<String>()));
    }
}
