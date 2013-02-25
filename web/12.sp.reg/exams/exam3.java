import java.util.*;
import tester.*;

class Merge {

    // Merge two sorted lists into a single sorted list.
    // ASSUME: list1, list2 are sorted according to comp.
    <T> ArrayList<T> merge(ArrayList<T> list1,
			   ArrayList<T> list2,
			   Comparator<T> comp) {

	ArrayList<T> result = new ArrayList<T>();
	Integer i1 = 0;
	Integer i2 = 0;

	while (!i1.equals(list1.size()) &&
	       !i2.equals(list2.size())) {

	    if (comp.compare(list1.get(i1), list2.get(i2)) < 0) {
		result.add(list1.get(i1));
		i1 = i1 + 1;
	    } else {
		result.add(list2.get(i2));
		i2 = i2 + 1;
	    }
	}
	while (!i1.equals(list1.size())) {
	    result.add(list1.get(i1));
	    i1 = i1 + 1;
	}
	while (!i2.equals(list2.size())) {
	    result.add(list2.get(i2));
	    i2 = i2 + 1;
	}

	return result;
    }
}

class LessThan implements Comparator<Integer> {
    public int compare(Integer i, Integer j) {
	return i.compareTo(j);
    }
}

