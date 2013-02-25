import tester.*;
import java.util.*;

interface Sort {
    // Sort given list in ascending order.
    ArrayList<Integer> sort(ArrayList<Integer> list);
}

class Selection implements Sort {

    // Sort given list in ascending order.
    // EFFECT: sorts the given list in-place.
    public ArrayList<Integer> sort(ArrayList<Integer> list) {
	Integer i = 0;
	while(!i.equals(list.size())) {
	    this.swap(list, i, this.findMin(list, i));
	    i++;
	}
	return list;
    }

    // EFFECT: swaps the elements at i and j in the list.
    void swap(ArrayList<Integer> list, Integer i, Integer j) {
	list.set(j, list.set(i, list.get(j)));
    }

    // Find the index of the first minimum element of the given list,
    // starting at the given index.
    Integer findMin(ArrayList<Integer> list, Integer idx) {
	Integer i = idx;
	Integer iMin = idx;
	while(!i.equals(list.size())) {
	    if (list.get(i) < list.get(iMin)) {
		iMin = i;
	    }
	    i++;
	}
	return iMin;
    }
}

class Insertion {
    
}

class Examples {
    Sort ssort = new Selection();
    Sort isort = new Insertion();

    ArrayList<Sort> sorts = new ArrayList<Sort>();{
	sorts.add(isort);
	sorts.add(ssort);
    }

    ArrayList<Integer> a = new ArrayList<Integer>();

    void init() {
	this.a.clear();
	this.a.add(7);
	this.a.add(1);
	this.a.add(9);
	this.a.add(4);
    }

    void testSorts(Tester t) {
	for(Sort s : this.sorts) {
	    this.init();
	    ArrayList<Integer> b = s.sort(this.a);
	    t.checkExpect(b.size(), 4);
	    t.checkExpect(b.get(0), 1);
	    t.checkExpect(b.get(1), 4);
	    t.checkExpect(b.get(2), 7);
	    t.checkExpect(b.get(3), 9);
	}
    }

    void testSelectionInPlace(Tester t) {
	this.init();
	ArrayList<Integer> b = this.ssort.sort(this.a);
	t.checkExpect(b == a, true);
    }
}

