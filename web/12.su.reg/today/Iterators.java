import tester.*;
import java.util.*;

class Posn implements Iterable<Integer> {
    Integer x;
    Integer y;
    Posn(Integer x, Integer y) {
	this.x = x;
	this.y = y;
    }

    public Iterator<Integer> iterator() {
	return new PosnIterator(this);
    }
}

class PosnIterator implements Iterator<Integer> {
    Posn p;
    Integer i; // i is in [0,2]
    PosnIterator(Posn p) {
	this.p = p;
	this.i = 0;
    }

    public void remove() {
	// This space is intentionally left blank.
    }

    public boolean hasNext() {
	return i < 2;
    }

    public Integer next() {
	this.i = this.i+1;
	if (this.i.equals(1)) {
	    return this.p.x;
	} else if (this.i.equals(2)) {
	    return this.p.y;
	} else {
	    throw new NoSuchElementException();
	}
    }
}

class Alg {
    Integer prod(Iterable<Integer> xs) {
	Integer acc = 1;
	for (Integer x : xs) {
	    acc = acc * x;
	}
	return acc;
    }
}



class Examples {
    void testPosnIterator(Tester t) {
	Posn p = new Posn(3,4);
	Iterator<Integer> ip = p.iterator();
	Iterator<Integer> jp = p.iterator();
	t.checkExpect(ip.hasNext(), true);
	t.checkExpect(ip.next(), 3);
	t.checkExpect(ip.hasNext(), true);
	t.checkExpect(ip.next(), 4);
	t.checkExpect(ip.hasNext(), false);
	t.checkExpect(jp.hasNext(), true);
    }

    void testProd(Tester t) {
	ArrayList<Integer> as = new ArrayList<Integer>();
	as.add(2);
	as.add(3);
	as.add(4);
	Posn p = new Posn(3, 4);
	Alg a = new Alg();
	t.checkExpect(a.prod(as), 24);
	t.checkExpect(a.prod(p), 12);
    }
}

