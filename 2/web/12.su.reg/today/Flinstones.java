import java.util.ArrayList;
import tester.*;

class Posn {
    Integer x;
    Integer y;
    Posn(Integer x, Integer y) {
	this.x = x;
	this.y = y;
    }
    /*
    public boolean equals(Object o) {
	return true;
    }
    */
}

class Examples {
    void testItOut(Tester t) {
	ArrayList<String> a = new ArrayList<String>() {{
		this.add("Fred");
		this.add("Wilma");
	    }};

	// a.add("Fred");
	// a.add("Wilma");
	a.add(2, "Barney");
	t.checkExpect(a.size(), 3);
	t.checkExpect(a.contains("Barney"), true);
	t.checkExpect(a.contains("Homer"), false);
	t.checkExpect(a.contains(7), false);
    }

    void testPosn(Tester t) {
	final Posn p = new Posn(3,4);
	ArrayList<Posn> ps = new ArrayList<Posn>() {{
		this.add(p);
	    }};
	t.checkExpect(ps.contains(p), true);
	t.checkExpect(ps.indexOf(p), 0);
	t.checkExpect(ps.indexOf(7), -1);
	t.checkExpect(ps.isEmpty(), false);
	t.checkExpect(ps.remove(0), p);
	t.checkExpect(ps.isEmpty(), true);
    }

    // EFFECT: swap elements at index i and j
    <E> void swap(int i, int j, ArrayList<E> as) {
	as.set(j, as.set(i, as.get(j)));
    }
}