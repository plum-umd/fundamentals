import tester.*;

interface Computation<X> {
    X run();
}

class Count implements Computation<Integer> {
    Integer i;
    Count() {
	this.i = 0;
    }

    public Integer run() {
	Integer j = this.i;
	this.i = i+1;
	return j;
    }      
}

class Thunk<X> implements Computation<X> {
    Delay<X> s;
    Thunk(Computation<X> c) {
	this.s = new Frozen<X>(c);
    }

    public X run() {
	this.s = this.s.force();
	return this.s.val();
    }
}

interface Delay<X> {
    Delay<X> force();
    X val();
}

class Frozen<X> implements Delay<X> {
    Computation<X> c;
    Frozen(Computation<X> c) {
	this.c = c;
    }
    public Delay<X> force() {
	return new Thaw<X>(this.c.run());
    }
    public X val() {
	return null;
    }
}

class Thaw<X> implements Delay<X> {
    X x;
    Thaw(X x) {
	this.x = x;
    }
    public Delay<X> force() {
	return this;
    }
    public X val() {
	return this.x;
    }
}

class Examples {
    void testThunk(Tester t) {
	Computation<Integer> count = new Count();
	t.checkExpect(count.run(), 0);
	t.checkExpect(count.run(), 1);
	t.checkExpect(count.run(), 2);
	t.checkExpect(count.run(), 3);
	Computation<Integer> thunk = new Thunk<Integer>(new Count());
	t.checkExpect(thunk.run(), 0);
	t.checkExpect(thunk.run(), 0);
    }
    
}