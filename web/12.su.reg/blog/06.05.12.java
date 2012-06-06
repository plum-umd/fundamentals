import java.util.*;
import tester.*;

interface Comparison<T> {
    // Is o1 "greater than" o2?
    Boolean compare(T o1, T o2);
}

class WrapComparison<T> implements Comparator<T> {
    Comparison<T> c;
    WrapComparison(Comparison<T> c) {
	this.c = c;
    }
    public int compare(T o1, T o2) {
	return c.compare(o1, o2)
	    ? 1
	    : c.compare(o2, o1)
	    ? -1
	    : 0;
    }
    /* EQUIVALENT TO:
	if (c.compare(o1, o2)) {
	    return 1;
	} else if (c.compare(o2, o1)) {
	    return -1;
	} else {
	    return 0;
	}
    */
    }
}

class IHateComparators {
    // Convert given comparison to a comparator.
    <T> Comparator<T> sonToRator(Comparison<T> c) {
	return new WrapComparison<T>(c);
    }
    // Convert given comparator to a comparison.
    // <T> Comparison<T> ratorToSon(Comparator<T> c);
}

// Order strings in increasing order of length
class StringLengthComparison implement Comparison<String> {
    public Boolean compare(String s1, String s2) {
	return s1.length() > s2.length();
    }
}

class SortAlgorithms {

    // Sort given list according to ordering by c.
    <T> ArrayList<T> sort(ArrayList<T> ls, Comparator<T> c) {
	isort(ls, c, ls.size());
	return ls;
    }

    // EFFECT: sort ls according to ordering of c.
    // Assume: ls is sorted from i to ls.size()
    // i in [0,ls.size()]
    void isort(ArrayList<T> ls, Comparator<T> c, Integer i) {
	if (i.equals(0)) {
	    // relax
	} else {
	    insert(ls, c, i-1);
	    isort(ls, c, i-1);
	}
    }

    // EFFECT: insert ls.get(i) into ls where ls is sorted from i+1 to ls.size()
    // i in [0,ls.size()-1]
    void insert(ArrayList<T> ls, Comparator<T> c, Integer i) {
	if (i.equals(ls.size() - 1)) {
	    // relax
	} else {
	    if (c.compare(ls.get(i), ls.get(i+1)) < 0) {
		// relax, have some coffee
	    } else {
		ls.set(i, ls.set(i+1, ls.get(i)));
		insert(ls, c, i+1);
	    }
	}
    }

}


class Examples {
    Comparison<String> strlength = new StringLengthComparison();
    Comparator<String> strlengthrator =
	new IHateComparators().sonToRator(strlength);

    void testStringLength(Tester t) {
	t.checkExpect(strlength.compare("", "a"), false);
	t.checkExpect(strlength.compare("a", ""), true);
	t.checkExpect(strlength.compare("", ""), false);

	t.checkExpect(strlengthrator.compare("", "a") < 0, true);
	t.checkExpect(strlengthrator.compare("a", "") > 0, true);
	t.checkExpect(strlengthrator.compare("", "") == 0, true);
    }
}


