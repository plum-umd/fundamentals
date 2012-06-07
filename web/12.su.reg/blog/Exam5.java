import tester.*;
import java.util.Comparator;
import java.util.ArrayList;

class Posn {
    Integer x;
    Integer y;
    Posn(Integer x, Integer y) {
	this.x = x;
	this.y = y;
    }
    // Produce distance to origin for this posn.
    Double dist() {
	return Math.sqrt((this.x * this.x) + (this.y * this.y));
    }
}

// Compare two posns so that closer to origin means "larger".
class PosnCompare implements Comparator<Posn> {
    public int compare(Posn a, Posn b) {
	Double ad = a.dist();
	Double bd = b.dist();
	return ad < bd ? 1 : ad.equals(bd) ? 0 : -1;
    }
}

class IsSorted {
    // Is given array list in ascending sorted order according to c?
    <T> Boolean isSorted(ArrayList<T> ls, Comparator<T> c) {
	if (ls.size() < 2) {
	    return true;
	} else {
	    return isSortedAcc(ls, c, 1);
	}
    }

    <T> Boolean isSortedAcc(ArrayList<T> ls, Comparator<T> c, Integer i) {
	if (i.equals(ls.size())) {
	    return true;
	} else {
	    return (c.compare(ls.get(i-1), ls.get(i)) <= 0)
		&& this.isSortedAcc(ls, c, i+1);
	}
    }
}

class Pair<A,B> {
    A left;
    B right;
    Pair(A left, B right) {
	this.left = left;
	this.right = right;
    }
}

class Lexi<A,B> implements Comparator<Pair<A,B>> {
    Comparator<A> ca;
    Comparator<B> cb;
    Lexi(Comparator<A> ca, Comparator<B> cb) {
	this.ca = ca;
	this.cb = cb;
    }

    // Compare x to y using lexicographic order.
    public int compare(Pair<A,B> x, Pair<A,B> y) {
	if (ca.compare(x.left, y.left) < 0) {
	    return -1;
	} else if (ca.compare(x.left, y.left) == 0) {
	    return cb.compare(x.right, y.right);
	} else {
	    return 1;
	}
    }
}

class Alg {
    <A,B> Comparator<Pair<A,B>> lexi(Comparator<A> ca,
				     Comparator<B> cb) {
	return new Lexi<A,B>(ca, cb);
    }
}

class Examples {
    Comparator<Posn> cp = new PosnCompare();

    void testPosnCompare(Tester t) {
	t.checkExpect(cp.compare(new Posn(3,4), new Posn(4,3)), 0);
	t.checkExpect(cp.compare(new Posn(3,4), new Posn(0,0)) < 0, true);
	t.checkExpect(cp.compare(new Posn(0,0), new Posn(3,4)) > 0, true);
    }

    void testIsSorted(Tester t) {
	IsSorted is = new IsSorted();
	ArrayList<Posn> ps = new ArrayList<Posn>();
	t.checkExpect(is.isSorted(ps, cp), true);
	ps.add(new Posn(5,6));
	t.checkExpect(is.isSorted(ps, cp), true);
	ps.add(new Posn(3,4));
	ps.add(new Posn(0,0));
	t.checkExpect(is.isSorted(ps, cp), true);
	ps.add(0, new Posn(0,0));
	t.checkExpect(is.isSorted(ps, cp), false);
    }

    void testLexi(Tester t) {
	Alg a = new Alg();
	Comparator<Pair<Posn,Posn>> cpp = a.lexi(cp, cp);
	t.checkExpect(cpp.compare(new Pair<Posn,Posn>(new Posn(3,4), new Posn(3,4)),
				  new Pair<Posn,Posn>(new Posn(3,4), new Posn(3,4))),
		      0);
	t.checkExpect(cpp.compare(new Pair<Posn,Posn>(new Posn(3,4), new Posn(3,4)),
				  new Pair<Posn,Posn>(new Posn(0,0), new Posn(3,4)))
		      < 0,
		      true);
	t.checkExpect(cpp.compare(new Pair<Posn,Posn>(new Posn(3,4), new Posn(0,0)),
				  new Pair<Posn,Posn>(new Posn(0,0), new Posn(3,4)))
		      < 0,
		      true);
	t.checkExpect(cpp.compare(new Pair<Posn,Posn>(new Posn(0,0), new Posn(3,4)),
				  new Pair<Posn,Posn>(new Posn(3,4), new Posn(0,0)))
		      > 0,
		      true);
	t.checkExpect(cpp.compare(new Pair<Posn,Posn>(new Posn(3,4), new Posn(0,0)),
				  new Pair<Posn,Posn>(new Posn(3,4), new Posn(3,4)))
		      > 0,
		      true);
	t.checkExpect(cpp.compare(new Pair<Posn,Posn>(new Posn(3,4), new Posn(3,4)),
				  new Pair<Posn,Posn>(new Posn(3,4), new Posn(0,0)))
		      < 0,
		      true);
    }
}
