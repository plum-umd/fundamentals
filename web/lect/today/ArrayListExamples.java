import tester.*;
import java.util.ArrayList;

// Useful methods for array lists
class ALMethods {
    // EFFECT: swapping the elements at index i and j
    <E> void swap(ArrayList<E> al, int i, int j) {
	al.set(j, al.set(i, al.get(j)));
    }

    Integer sum(ArrayList<Integer> al) {
	return sumTraversal(new ArrTraversal(al));
	// Think about this
	// return this.sumAcc(al, 0);
    }

    Integer sumAcc(ArrayList<Integer> al, Integer i) {
	if (i.equals(al.size())) {
	    return 0;
	} else {
	    return al.get(i) + this.sumAcc(al, i+1);
	}
    }

    Integer sumTraversal(Traversal<Integer> t) {
	if (t.emptyHuh()) {
	    return -0;
	} else {
	    return t.getFirst() + sumTraversal(t.getRest());
	}
    }
}

class ArrTraversal<T> implements Traversal<T> {
    ArrayList<T> al;

    ArrayTraversal(ArrayList<T> al) {
	this.al = al;
    }

    public Boolean emptyHuh() {
	return al.size().equals(0);		
    }

    public T getFirst() {
	return al.get(0);
    }

    public Traversal<T> getRest() {
	???
	// Think about this.
    }
}

// Represents the ordered Traversal of some data 
interface Traversal<W>{
    // Is it empty
    Boolean emptyHuh();
    // Get the first of this Traversal (a W)
    W getFirst();
    // Get the rest of this Traversal (another Traversal)
    Traversal<W> getRest();
}

interface Stack<T> {
    void push(T t);
    T pop();
    T peek();
}

interface Queue<T> {
    void enqueue(T t);
    T dequeue();    
    T peek();
}

class Pancake {
    String topping;
    Pancake(String topping) {
	this.topping = topping;
    }
}

class ArrStack<T> implements Stack<T> {
    ArrayList<T> elements;

    // Construct an empty stack
    ArrStack() {
	this.elements = new ArrayList<T>();
    }

    public void push(T t) {
	this.elements.add(t);
    }

    // Produce the last element pushed onto this stack.
    // EFFECT: remove the last element pushed.
    public T pop() {
	return this.elements.remove(this.elements.size()-1);
    }

    // Produce the last element pushed onto this stack.
    public T peek() {
	return this.elements.get(this.elements.size()-1);
    }
}

class ArrQueue<T> implements Queue<T> {
    ArrayList<T> elements;
    ArrQueue() {
	this.elements = new ArrayList<T>();
    }

    public void enqueue(T t) {
	this.elements.add(t);
    }

    public T dequeue() {
	return this.elements.remove(0);
    }

    public T peek() {
	return this.elements.get(0);
    }
}


class Examples {
    ALMethods m = new ALMethods();
    
    void testSwap(Tester t) {
	ArrayList<String> a = new ArrayList<String>();
	a.add("a");
	a.add("b");
	m.swap(a, 0, 1);
	ArrayList<String> b = new ArrayList<String>();
	b.add("b");
	b.add("a");
	t.checkExpect(a, b);
    }
    

    void testConstructArrayList(Tester t) {
	ArrayList<String> a = new ArrayList<String>();
        a.add("a");
	a.add("b");
	a.add("c");
	ArrayList<String> b = new ArrayList<String>();
	b.add("a");
	b.add("b");
	b.add("c");
	ArrayList<String> f = new ArrayList<String>(a);
	t.checkExpect(a, b);
	t.checkExpect(a.get(0), "a");
	t.checkExpect(a.get(1), "b");
	t.checkExpect(a.get(2), "c");
	// t.checkExpect(a.get(4), "something");
	a.add(1, "d");
	t.checkExpect(a.get(0), "a");
	t.checkExpect(a.get(1), "d");
	t.checkExpect(a.get(2), "b");
	t.checkExpect(a.get(3), "c");
	a.addAll(b);
	t.checkExpect(a.get(0), "a");
	t.checkExpect(a.get(1), "d");
	t.checkExpect(a.get(2), "b");
	t.checkExpect(a.get(3), "c");
	t.checkExpect(a.get(4), "a");
	t.checkExpect(a.get(5), "b");
	t.checkExpect(a.get(6), "c");
	a.clear();
	t.checkExpect(a, new ArrayList<String>());
	t.checkExpect(a.contains("a"), false);
	t.checkExpect(a.isEmpty(), true);
	a.addAll(b);
	t.checkExpect(a.contains("c"), true);
	a.addAll(b); // [a,b,c,a,b,c]
	t.checkExpect(a.indexOf("c"), 2);
	t.checkExpect(a.isEmpty(), false);
	t.checkExpect(a.lastIndexOf("c"), 5);
	t.checkExpect(a.size(), 6);
	t.checkExpect(a.remove(5), "c"); // [a,b,c,a,b]
	t.checkExpect(a.size(), 5);
	t.checkExpect(a.remove("a"), true);  // [b,c,a,b]
	t.checkExpect(a.get(0), "b");
	t.checkExpect(a.containsAll(b), true);
	//a.removeRange(1,3); // [b,b]
	t.checkExpect(a.set(1, "d"), "c");  // [b, d, a, b]
	t.checkExpect(a.get(1), "d");
	t.checkExpect(a.containsAll(b), false);
	a.retainAll(b); // [b, a, b]
	t.checkExpect(a.size(), 3);
	t.checkExpect(a.get(1), "a");
	ArrayList<String> c = (ArrayList<String>)a.clone();  // [b,a,b]
	ArrayList<String> d = a;
	t.checkExpect(c.get(0), "b");
	t.checkExpect(d.get(0), "b");
	a.add("c"); // [b,a,b,c]
	t.checkExpect(a.get(3), "c");
	t.checkExpect(d.get(3), "c");
	t.checkExpect(c.size(), 3); // [b,a,b]
    }

    void testClone(Tester t) {
	ArrayList<ArrayList<Integer>> a = new ArrayList<ArrayList<Integer>>();
	ArrayList<Integer> x = new ArrayList<Integer>();
	x.add(1);
	x.add(2);
	x.add(3);
	a.add(x);
	ArrayList<ArrayList<Integer>> b = (ArrayList<ArrayList<Integer>>)a.clone();
	x.set(0, 42);
	t.checkExpect(b.get(0).get(0), 42);
    }
    
    void testSum(Tester t) {
	ArrayList<Integer> is = new ArrayList<Integer>() {{
		this.add(7);
		this.add(3);
		this.add(2);
	    }};
	t.checkExpect(m.sum(is), 12);
    }    

    void testStack(Tester t) {
	Pancake pb = new Pancake("butter");
	Pancake ps = new Pancake("syrup");
	Stack<Pancake> notSoShort = new ArrStack<Pancake>();
	notSoShort.push(pb);
	notSoShort.push(ps);
	t.checkExpect(notSoShort.peek(), ps);
	t.checkExpect(notSoShort.pop(), ps);
	t.checkExpect(notSoShort.peek(), pb);
    }

    void testQueue(Tester t) {
	Pancake pb = new Pancake("butter");
	Pancake ps = new Pancake("syrup");
	Queue<Pancake> notSoShort = new ArrQueue<Pancake>();
	notSoShort.enqueue(pb);
	notSoShort.enqueue(ps);
	t.checkExpect(notSoShort.peek(), pb);
	t.checkExpect(notSoShort.dequeue(), pb);
	t.checkExpect(notSoShort.peek(), ps);
    }

}
