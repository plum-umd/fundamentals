import tester.*;

// Safe casting approach to structural equality.

// Represents a geometric shape
interface Shape {
    // Is this shape the same as that shape?
    Boolean same(Shape that);

    // Is this shape a square?
    Boolean isSq();
    // Is this shape a circle?
    Boolean isCirc();
    // Convert this shape to a square
    Sq toSq();
    // Convert this shape to a circle
    Circ toCirc();
}

abstract class AShape implements Shape {
    public Boolean isSq() { return false; }
    public Boolean isCirc() { return false; }
    public Sq toSq() {
	throw new RuntimeException("BARF");
    }
    public Circ toCirc() {
	throw new RuntimeException("BARF");
    }
}

// Represent a square
class Sq extends AShape {
    Integer size;
    Sq(Integer size) {
	this.size = size;
    }

    // Is this square the same as that shape?
    public Boolean same(Shape that) {
	return that.isSq()
	    && this.size.equals(that.toSq().size);
    }

    // Is this square a square?
    public Boolean isSq() { return true; }
    // Convert this square to a square
    public Sq toSq() { return this; }
}

// Represents a circle
class Circ extends AShape {
    Integer radius;
    Circ(Integer radius) {
	this.radius = radius;
    }

    // Is this circle a circle?
    public Boolean isCirc() { return true; }
    // Convert this circle to a circle
    public Circ toCirc() { return this; }

    // Is this circle the same as that shape?
    public Boolean same(Shape that) {
	return that.isCirc()
	    && this.radius.equals(that.toCirc().radius);
    }
}

class Examples {
    Examples() {}

    void testSame(Tester t) {
	t.checkExpect(new Sq(5).same(new Sq(5)), true);
	t.checkExpect(new Sq(5).same(new Sq(7)), false);
	t.checkExpect(new Circ(5).same(new Circ(5)), true);
	t.checkExpect(new Circ(5).same(new Circ(7)), false);
	t.checkExpect(new Circ(5).same(new Sq(5)), false);
    }
}
