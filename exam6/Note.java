import tester.*;
import java.util.*;

class Note {
    String txt;
    List<Note> subnotes;
    
    Note(String txt) {
	this(txt, new Empty<Note>());
    }

    Note(String txt, List<Note> subnotes) {
	this.txt = txt;
	this.subnotes = subnotes;
    }

    <R> R accept(NoteVisitor<R> v) {
	return v.visitNote(this.txt, this.subnotes);
    }
}

interface NoteVisitor<R> {
    R visitNote(String txt, List<Note> subnotes);
}

interface List<X> {
    <R> R accept(ListVisitor<X,R> v);
}

interface ListVisitor<X,R> {
    R visitEmpty();
    R visitCons(X first, List<X> rest);
}

class Empty<X> implements List<X> {
    Empty() {}
    public <R> R accept(ListVisitor<X,R> v) {
	return v.visitEmpty();
    }
}

class Cons<X> implements List<X> {
    X first;
    List<X> rest;

    Cons(X first, List<X> rest) {
	this.first = first;
	this.rest = rest;
    }

    public <R> R accept(ListVisitor<X,R> v) {
	return v.visitCons(this.first, this.rest);
    }
}

class Outline implements NoteVisitor<ArrayList<String>> {
    String prefix;
    Integer secnum;
    ArrayList<String> output;

    Outline() {
	this("1.", 1, new ArrayList<String>());
    }
    Outline(String prefix, Integer secnum, ArrayList<String> output) {
	this.prefix = prefix;
	this.secnum = secnum;
	this.output = output;
    }

    public ArrayList<String> visitNote(String txt, List<Note> subnotes) {
	this.print(txt);
	subnotes.accept(new OutlineList(this.prefix, 1, this.output));
	return this.output;
    }

    public void print(String txt) {
	this.output.add(this.prefix.concat(" ".concat(txt)));
    }
}

class OutlineList implements ListVisitor<Note,Void> {
    String prefix;
    Integer secnum;
    ArrayList<String> output;
    OutlineList(String prefix, Integer secnum, ArrayList<String> output) {
	this.prefix = prefix;
	this.secnum = secnum;
	this.output = output;
    }
    
    public Void visitEmpty() { return null; }
    public Void visitCons(Note first, List<Note> rest) {
	first.accept(new Outline(this.prefix.concat(this.secnum.toString().concat(".")), this.secnum, this.output));
	rest.accept(new OutlineList(this.prefix, this.secnum+1, this.output));
	return null;
    }       
}

// Nesting Depth with recursion
class NestDepthR implements NoteVisitor<Integer> {
    NestDepthR () {}
    public Integer visitNote(String txt, List<Note> subnotes) {
	return 1 + subnotes.accept(new NestDepthListR());
    }
}

class NestDepthListR implements ListVisitor<Note,Integer> {
    NestDepthListR () {}
    public Integer visitEmpty() {
	return 0;
    }
    public Integer visitCons(Note first, List<Note> rest) {
	return Math.max(first.accept(new NestDepthR()),
			rest.accept(this));
    }
}

// Nesting Depth with accumulator
class NestDepth implements NoteVisitor<Integer> {
    Integer a;
    NestDepth() {
	this(0);
    }
    NestDepth(Integer a) {
	this.a = a;
    }

    public Integer visitNote(String txt, List<Note> subnotes) {
	return subnotes.accept(new NestDepthList(this.a+1));
    }
}

class NestDepthList implements ListVisitor<Note,Integer> {
    Integer a;
    NestDepthList(Integer a) {
	this.a = a;
    }
    public Integer visitEmpty() {
	return this.a;
    }
    public Integer visitCons(Note first, List<Note> rest) {
	return Math.max(first.accept(new NestDepth(this.a)),
			rest.accept(this));
    }
}

class Examples {
    List<Note> mt = new Empty<Note>();

    Note wat = new Note("WAT?");
    Note unmotivating = new Note("unmotivating");
    Note motivations =
	new Note("Motivations", new Cons<Note>(unmotivating, mt));
    Note overview =
	new Note("Overview",
		 new Cons<Note>(wat,
				new Cons<Note>(motivations, mt)));
					       
    Note middle = new Note("Middle (fell asleep)");
    Note conclusions = new Note("Conclusions");
    Note budget = 
	new Note("Budget",
		 new Cons<Note>(new Note("very large"),
				new Cons<Note>(new Note("lots of BU undergrads"), mt)));
    Note nsf = 
	new Note("NSF Proposal 1218390",
		 new Cons<Note>(overview,
				new Cons<Note>(middle,
					       new Cons<Note>(conclusions,
							      new Cons<Note>(budget, mt)))));

    
    void testNestDepth(Tester t) {
	t.checkExpect(new Note("flat").accept(new NestDepth()), 1);
	t.checkExpect(nsf.accept(new NestDepth()), 4);
    }

    void testNestDepthR(Tester t) {
	t.checkExpect(new Note("flat").accept(new NestDepthR()), 1);
	t.checkExpect(nsf.accept(new NestDepthR()), 4);
    }

    void testOutline(Tester t) {
	ArrayList<String> oline =
	    new ArrayList<String>(Arrays.asList("1. NSF Proposal 1218390",
						"1.1. Overview",
						"1.1.1. WAT?",
						"1.1.2. Motivations",
						"1.1.2.1. unmotivating",
						"1.2. Middle (fell asleep)",
						"1.3. Conclusions",
						"1.4. Budget",
						"1.4.1. very large",
						"1.4.2. lots of BU undergrads"));
	t.checkExpect(nsf.accept(new Outline()), oline);
    }

    
}