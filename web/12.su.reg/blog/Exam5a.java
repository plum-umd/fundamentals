import tester.*;
import java.util.Comparator;
import java.util.ArrayList;

class Square {
    Integer width;
    Integer height;
    Square(Integer width, Integer height) {
	this.width = width;
	this.height = height;
    }
    // Produce area of this square
    Integer area() {
	return this.width * this.height;
    }
}

// Compare two squares so that smaller area means "larger".
class SquareCompare implements Comparator<Square> {
    public int compare(Square a, Square b) {
	Integer ad = a.area();
	Integer bd = b.area();
	return ad < bd ? 1 : ad.equals(bd) ? 0 : -1;
    }
}

class NoSameNeighbors {
    // Is given array list in ascending sorted order according to c?
    <T> Boolean noSameNeighbors(ArrayList<T> ls, Comparator<T> c) {
	if (ls.size() < 2) {
	    return true;
	} else {
	    return noSameAcc(ls, c, 1);
	}
    }

    <T> Boolean noSameAcc(ArrayList<T> ls, Comparator<T> c, Integer i) {
	if (i.equals(ls.size())) {
	    return true;
	} else {
	    return !(c.compare(ls.get(i-1), ls.get(i)) == 0)
		&& this.noSameAcc(ls, c, i+1);
	}
    }
}

class Dict<A> implements Comparator<ArrayList<A>> {
    Comparator<A> ca;
    Dict(Comparator<A> ca) {
	this.ca = ca;
    }

    // Compare x and y using dictionary ordering.
    public int compare(ArrayList<A> x, ArrayList<A> y) {
        return compareAcc(x, y, 0);
    }

    int compareAcc(ArrayList<A> x, ArrayList<A> y, Integer i) {
	if (i.equals(x.size())) {
	    return x.size() - y.size();
	} else if (i.equals(y.size())) {
	    return 1;
	} else {
	    int first = ca.compare(x.get(i), y.get(i));
	    if (first == 0) {
		return compareAcc(x, y, i+1);
	    } else {
		return first;
	    }
	}
    }
}

class Alg {
    <A> Comparator<ArrayList<A>> dict(Comparator<A> ca) {
	return new Dict<A>(ca);
    }
}

class Examples {

    Comparator<Square> cs = new SquareCompare();

    void testSquareCompare(Tester t) {
	t.checkExpect(cs.compare(new Square(3,4), new Square(4,3)), 0);
	t.checkExpect(cs.compare(new Square(3,4), new Square(0,0)) < 0, true);
	t.checkExpect(cs.compare(new Square(0,0), new Square(3,4)) > 0, true);
    }

    void testIsSorted(Tester t) {
	NoSameNeighbors is = new NoSameNeighbors();
	ArrayList<Square> ss = new ArrayList<Square>();
	t.checkExpect(is.noSameNeighbors(ss, cs), true);
	ss.add(new Square(5,6));
	t.checkExpect(is.noSameNeighbors(ss, cs), true);
	ss.add(new Square(3,4));
	ss.add(new Square(8,4));
	t.checkExpect(is.noSameNeighbors(ss, cs), true);
	ss.add(new Square(4,8));
	t.checkExpect(is.noSameNeighbors(ss, cs), false);
    }

    void testDict(Tester t) {
	Alg a = new Alg();
	Comparator<ArrayList<Square>> csd = a.dict(cs);
	t.checkExpect(csd.compare(new ArrayList<Square>(),
				  new ArrayList<Square>()),
		      0);
	t.checkExpect(csd.compare(new ArrayList<Square>() {{
			this.add(new Square(3,4));
		    }},
		new ArrayList<Square>()) > 0,
	    true);
	t.checkExpect(csd.compare(new ArrayList<Square>(),
				  new ArrayList<Square>() {{
					  this.add(new Square(3,4));
				      }}) < 0,
		      true);
	t.checkExpect(csd.compare(new ArrayList<Square>() {{
			this.add(new Square(3,4));
		    }},
		new ArrayList<Square>() {{
			this.add(new Square(3,4));
		    }}),
	    0);

	t.checkExpect(csd.compare(new ArrayList<Square>() {{
			this.add(new Square(3,3));
		    }},
		new ArrayList<Square>() {{
			this.add(new Square(3,4));
		    }}) > 0,
	    true);
    }
}
