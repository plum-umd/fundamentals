import tester.*;
import java.util.*;

class MapIterator<T,U> implements Iterator<U> {
  Function<T,U> f;
  Iterator<T> i;
  MapIterator(Function<T,U> f, Iterator<T> i) {
   this.f = f; this.i = i;
  }

  public boolean hasNext() { return i.hasNext(); }
  public U next() { return f.call(i.next()); }
  public void remove() { throw new RuntimeException("remove"); }
}

interface Function<T,U> {
  U call(T x);
}

class RangeIterator implements Iterator<Integer> {
  Integer lo;
  Integer hi;
  
  RangeIterator (Integer lo, Integer hi) { 
    this.lo = lo; this.hi = hi; 
  }

  public boolean hasNext() { return this.lo <= this.hi; }

  public Integer next() { this.lo = this.lo + 1; return this.lo - 1; }

  public void remove() { throw new RuntimeException("remove"); }

}

class Square implements Function<Integer,Integer> {
  public Integer call(Integer x) { return x * x; }
} 


class Test {
    Integer sumSquares(Integer a, Integer b) {
	Iterator<Integer> i = new MapIterator<Integer,Integer>(new Square(), 
							       new RangeIterator(a,b));
	Integer sum = 0;
	while (i.hasNext()) { sum += i.next(); }
	return sum;
    }
    
    void test(Tester t) {
	//> Tests 2pts
	RangeIterator ri = new RangeIterator(0,2);
	t.checkExpect(ri.hasNext(), true);
	t.checkExpect(ri.next(), 0);
	t.checkExpect(ri.next(), 1);
	t.checkExpect(ri.hasNext(), true);
	t.checkExpect(ri.next(), 2);
	t.checkExpect(ri.hasNext(), false);
	Iterator<Integer> i = new MapIterator<Integer,Integer>(new Square(), 
							       new RangeIterator(1,5));
	t.checkExpect(i.next(), 1);
	t.checkExpect(i.next(), 4);
	t.checkExpect(i.next(), 9);
	t.checkExpect(i.hasNext(), true);
	//> Tests 1pt
	t.checkExpect(sumSquares(1,2), 5);
	t.checkExpect(sumSquares(0,0), 0);
	t.checkExpect(sumSquares(10,10), 100);
    }
	
}

