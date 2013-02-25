import tester.*;

import java.util.ArrayList;

// Represents a dinosaur.
class Dino {
    String eyecolor;
    Integer teeth;
    
    Dino(String eyecolor, Integer teeth) {
	this.eyecolor = eyecolor;
	this.teeth = teeth;
    }
}

class LectureExamples {

    ArrayList<Dino> alist = new ArrayList<Dino>();

    Dino d1 = new Dino("blue", 2);
    Dino d2 = new Dino("yellow", 70);
    Dino d3 = new Dino("black", 6);
    Dino d4 = new Dino("chartreuse", 50);
    Dino d5 = new Dino("red", 8);
    
    // Methods of ArrayList
    // void clear()
    // boolean isEmpty()
    // int size()
    // boolean add(E element)
    // void add(int index, E element)
    // E get(int index)
    // E set(int index, E element)
    // E remove(int index)

    void testAll(Tester t) {
	t.checkExpect(this.alist.isEmpty(), true);
	t.checkExpect(this.alist.size(), 0);
	this.alist.add(this.d1);
	t.checkExpect(this.alist.isEmpty(), false);
	t.checkExpect(this.alist.size(), 1);
	t.checkExpect(this.alist.get(0), this.d1);
	this.alist.add(this.d2);
	t.checkExpect(this.alist.get(0), this.d1);
	t.checkExpect(this.alist.get(1), this.d2);
	this.alist.clear();
	t.checkExpect(this.alist.isEmpty(), true);
	this.alist.add(this.d1);
	this.alist.add(this.d2);
	this.alist.add(0, this.d3);
	t.checkExpect(this.alist.get(0), this.d3);
	
	this.alist.add(this.d4);
	// this.alist.add(5);
	t.checkExpect(this.alist.get(0).eyecolor, "black");

	// d3, d1, d2, d4
	t.checkExpect(this.alist.set(0, this.d5), this.d3);
	// d5, d1, d2, d4
	t.checkExpect(this.alist.get(0), this.d5);
	this.swap(this.alist, 0, 3);
	t.checkExpect(this.alist.get(0), this.d4);
	t.checkExpect(this.alist.get(3), this.d5);
	// d4, d1, d2, d5
	this.alist.remove(1);
	// d4, d2, d5
	t.checkExpect(this.alist.size(), 3);
	t.checkExpect(this.alist.get(1), this.d2);


	this.alist.clear();
	// 1, 3, 5, 4, 2
	this.alist.add(this.d1);
	this.alist.add(this.d3);
	this.alist.add(this.d5);
	this.alist.add(this.d4);
	this.alist.add(this.d2);
	
	t.checkExpect(this.search(this.alist, 50), this.d4);
	t.checkExpect(this.search(this.alist, 6), this.d3);
	t.checkExpect(this.search(this.alist, 7), this.d3);
	
    }

    // EFFECT: swap the elements at index i and j.
    void swap(ArrayList<Dino> ds, int i, int j) {
	ds.set(j, ds.set(i, ds.get(j)));	
    }


    // Produce a dinosaur with the given number of teeth from the list.
    Dino search(ArrayList<Dino> ds, int teeth) {
	return this.searchAcc(ds, teeth, 0, ds.size());
    }

    Dino searchAcc(ArrayList<Dino> ds, int teeth, int lo, int hi) {
	int middle = (lo+hi)/2;

	if (lo > hi) {
	    throw new RuntimeException("No such element");
	}
	
	if (ds.get(middle).teeth == teeth) {
	    return ds.get(middle);
	} else if (teeth > ds.get(middle).teeth) {
	    return this.searchAcc(ds, teeth, middle+1, hi);
	} else {
	    return this.searchAcc(ds, teeth, lo, middle-1);
	}	
    }

}