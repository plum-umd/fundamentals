import tester.*;
import java.util.*;

class Pair<X,Y> extends Object {
    X x;
    Y y;
    Pair(X x, Y y) {
	this.x = x;
	this.y = y;
    }

    public boolean equals(Object that) {
	Pair<X,Y> p = (Pair)that;
	return this.x.equals(p.x)
	    && this.y.equals(p.y);
    }

    public int hashCode() {
	return this.x.hashCode() + this.y.hashCode();
    }
}

class Posn extends Object {
    Integer x;
    Integer y;
    Posn(Integer x, Integer y) {
	this.x = x;
	this.y = y;
    }

    public boolean equals(Object that) {
	if (that instanceof Posn) {
	    Posn p = (Posn)that;
	    return this.x.equals(p.x)
		&& this.y.equals(p.y);
	} else {
	    return false;
	}
    }

    public int hashCode() {
	return this.x + this.y;
    }

}

class Examples {
    Pair<Posn,Posn> p = new Pair<Posn,Posn>(new Posn(3, 4), new Posn(5, 6));
    Pair<Posn,Posn> q = new Pair<Posn,Posn>(new Posn(3, 4), new Posn(5, 6));


    void testEquals(Tester t) {
	t.checkExpect(new Posn(3, 4).equals(new Posn(3, 4)), true);
	t.checkExpect(new Posn(3, 4).hashCode(), new Posn(3, 4).hashCode());
	t.checkExpect(p.equals(q), true);
	t.checkExpect(p.hashCode(), q.hashCode());
	t.checkExpect(new Posn(3,4).equals("(3,4)"), false);
    }
}