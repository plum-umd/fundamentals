import tester.*;
import java.util.ArrayList;

interface List<T> {
    void setTail(List<T> t);
    void setTailAcc(List<T> t, Cons<T> c);

    Integer length();
    Integer lengthAcc(ArrayList<List<T>> seen);
}

class MT<T> implements List<T> {
    public void setTail(List<T> t) {
	throw new RuntimeException("Can't do that!");
    }

    public Integer length() {
	return 0;
    }
    public Integer lengthAcc(ArrayList<List<T>> seen) {
	return 0;
    }
    // Accumlator invariant : c is the previous 
    // cons in the list
    // Purpose : set c's rest to the given list
    public void setTailAcc(List<T> t, Cons<T> c) {
	c.rst = t;
    }
}
class Cons<T> implements List<T> {
    T fst;
    List<T> rst;

    Cons(T f, List<T> r) { this.fst = f; this.rst = r; }

    public Integer length() {
	return this.lengthAcc(new ArrayList<List<T>>());
    }

    public Integer lengthAcc(ArrayList<List<T>> seen) {
	if (seen.contains(this)) 
	    return 0;

	// for (List<T> s : seen)
	//     if (s == this) {
	// 	return 0;
	//     }
	else {
	    seen.add(this);
	    return 1 + this.rst.lengthAcc(seen);
	}
    }


    public void setTail(List<T> t) {
	this.rst.setTailAcc(t, this);
    }
    public void setTailAcc(List<T> t, Cons<T> c) {
	this.rst.setTailAcc(t, this);
    }
}

class Examples {
    void testSetTail(Tester t) {
	List<Integer> one = 
	    new Cons<Integer>(1, new MT<Integer>());
	List<Integer> two = 
	    new Cons<Integer>(2, new MT<Integer>());
	one.setTail(two);
	t.checkExpect(one, one);
	t.checkExpect(one, 
		      new Cons<Integer>(1,
			 new Cons<Integer>(2,
			   new MT<Integer>())));
	one.setTail(one);
	List<Integer> onen = 
	    new Cons<Integer>(1, new MT<Integer>());
	List<Integer> twon = 
	    new Cons<Integer>(2, new MT<Integer>());
	t.checkExpect(one, one);
	onen.setTail(twon);
	onen.setTail(onen);
	t.checkExpect(one.length(), 2);
	t.checkExpect(one.length(), onen.length());
    }
}