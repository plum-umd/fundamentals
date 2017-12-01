/*
 * THIS CODE IS BROKEN
 * It's only a snapshot of what we came up with in class and we
 * identified serious problems with it.
 */

class Quicksort {

    // Sort given list according to ordering by c.
    <T> ArrayList<T> sort(ArrayList<T> ls, Comparator<T> c) {
	qsort(ls, new ArrayList<T>(ls), c, 0, ls.size());

    }

    // Assume: lo <= hi, lo and hi-1 are valid indices into ls
    <T> void qsort(ArrayList<T> ls,
		   ArrayList<T> scratch,
		   Comparator<T> c,
		   Integer lo,
		   Integer hi) {
	if (lo.equals(hi)) {
	    // done
	} else {
	    Integer pivotIndex = (hi - lo) / 2;
	    T pivot = ls.get(pivotIndex);
	    partition(ls, scratch, pivot, c, lo, hi, lo, hi);
	    Integer i = scratch.getIndex(pivot);
	    qsort(scratch, ls, c, 0, i);
	    qsort(scratch, ls, c, i+1, scratch.size());
	}
    }

    <T> void partition(ArrayList<T> ls,
		       ArrayList<T> scratch,
		       T pivot,
		       Comparator<T> c,
		       Integer lo,
		       Integer hi,
		       Integer endSmall,
		       Integer startBig) {
	if (lo.equals(hi)) {
	    scratch.set(endSmall, pivot);
	} else if (ls.get(0) == pivot) {
	    partition(ls, scratch, pivot, c, lo+1, hi, endSmall, startBig)
	} else if (c.compare(ls.get(lo), pivot) < 0) {
	    scratch.set(endSmall, ls.get(lo));
	    partition(ls, scratch, pivot, c, lo+1, hi, endSmall+1, startBig)
	} else {
	    scratch.set(startBig-1, ls.get(lo));
	    partition(ls, scratch, pivot, c, lo+1, hi, endSmall, startBig-1)
	}
    }
}