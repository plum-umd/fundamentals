import tester.*;
import java.util.*;

interface Sort {
    // Sort given list in ascending order.
    ArrayList<Integer> sort(ArrayList<Integer> list);
}

class Selection implements Sort {

    // Sort given list in ascending order.
    // EFFECT: given list is sorted in-place.
    public ArrayList<Integer> sort(ArrayList<Integer> list) {
	Integer i = 0;
	while (!i.equals(list.size())) {
	    this.swap(list, i, this.findMin(list, i));
	    i++;
	}
	return list;
    }

    // Starting at idx, find index of minimum element in list.
    Integer findMin(ArrayList<Integer> list, Integer idx) {
	Integer i = idx;
	Integer min = list.get(i);
	while (!i.equals(list.size())) {
	    if (list.get(i) < min) {
		min = list.get(i);
	    }
	    i++;
	}
	return min;
    }

    // EFFECT: Swap elements at index i and j in list.
    void swap(ArrayList<Integer> list, Integer i, Integer j) {
	list.set(j, list.set(i, list.get(j)));
    }
}

class Insertion implements Sort {

    // Sort given list in ascending order.
    // EFFECT: given list is sorted in-place.
    public ArrayList<Integer> sort(ArrayList<Integer> list) {
	Integer i = 0;
	while (!i.equals(list.size())) {
	    this.insert(i, list);
	    i++;
	}
	return list;
    }

    // EFFECT: Insert element of list at idx into
    // sorted prefix of list.
    void insert(Integer idx, ArrayList<Integer> list) {
        Integer i = 0;
        while (i < idx && list.get(idx) > list.get(i))
            i++;
        list.add(i, list.remove(idx.intValue()));
    }
}


class Examples {
    Sort isort = new Insertion();
    Sort ssort = new Selection();

    ArrayList<Sort> sorts = new ArrayList<Sort>();{
	sorts.add(isort);
	sorts.add(ssort);
    }

    ArrayList<Integer> a = new ArrayList<Integer>();

    void init() {
	a.clear();
	a.add(7);
	a.add(1);
	a.add(9);
	a.add(4);
    }

    void testSorts(Tester t) {
	for(Sort s : this.sorts) {
	    this.init();
	    ArrayList<Integer> b = s.sort(a);
	    t.checkExpect(b.size(), 4);
	    t.checkExpect(b.get(0), 1);
	    t.checkExpect(b.get(1), 4);
	    t.checkExpect(b.get(2), 7);
	    t.checkExpect(b.get(3), 9);
	}
    }

    void testInsertionInPlace(Tester t) {
	this.init();
	ArrayList<Integer> b = this.isort.sort(a);
	t.checkExpect(a == b, true);
    }
}
