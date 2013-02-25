import tester.*;
import java.util.*;

class HalfOpen implements Iterable<Integer>, Iterator<Integer> {
    Integer lo;
    Integer hi;
    HalfOpen(Integer lo, Integer hi) {
	this.lo = lo;
	this.hi = hi;
    }

    public Iterator<Integer> iterator() { return this; }
    public Integer next() {
	lo++;
	return lo - 1;
    }
    public boolean hasNext() {
	return this.lo < this.hi;
    }
    public void remove() {}
}

class Examples {
    public static void main(String[] in) {
	// void testFact(Tester t) {
	Integer acc = 1;
	for (Integer i : new HalfOpen(1,6)) {
	    acc = acc * i;
	}
	System.out.println(acc);
    }
}