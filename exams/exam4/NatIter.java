import java.util.*;
import tester.*;

class NaturalIterator implements Iterator<Integer>, Iterable<Integer> {
    Integer i;
    Integer n;
    NaturalIterator(Integer n) {
	this.i = 0;
	this.n = n;
    }

    public boolean hasNext() {
	return !this.i.equals(n);
    }

    public Integer next() {
	Integer j = this.i;
	this.i = i + 1;
	return j;
    }

    public Iterator<Integer> iterator() {
	return this;
    }

    public void remove() {}
}

class Examples {
    void testFact(Tester t) {
	Integer acc = 1;
	Iterator<Integer> is = new NaturalIterator(5);
	for(Integer i : is) {
	    acc = i * acc;
	    System.out.println(i);
	}
	t.checkExpect(acc, 125);
    }
}