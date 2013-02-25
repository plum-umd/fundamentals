import java.util.ArrayList;
import tester.*;

// boolean add(E)
// void add(int, E)
// void clear()
// E get(int)
// boolean isEmpty()
// E remove(int)
// E set(int, E)
// int size()

class Examples {
    ArrayList<Integer> is = new ArrayList<Integer>();

    // Swap values at position i and j in ts.
    <T> void swap(ArrayList<T> ts, Integer i, Integer j) {
	ts.set(j, ts.set(i, ts.get(j)));
    }
    
    void testAL(Tester t) {
	t.checkExpect(is.isEmpty(), true);
	is.add(5);
	t.checkExpect(is.isEmpty(), false);
	t.checkExpect(is.get(0), 5);
	is.clear();
	t.checkExpect(is.isEmpty(), true);
	is.add(5);
	is.add(3);
	t.checkExpect(is.get(0), 5);
	t.checkExpect(is.get(1), 3);
	is.add(1, 8);
	t.checkExpect(is.get(0), 5);
	t.checkExpect(is.get(1), 8);
	t.checkExpect(is.get(2), 3);
	is.remove(1);
	t.checkExpect(is.get(0), 5);
	t.checkExpect(is.get(1), 3);
	is.set(1, 17);
	t.checkExpect(is.get(0), 5);
	t.checkExpect(is.get(1), 17);
	t.checkExpect(is.size(), 2);
    }
}

