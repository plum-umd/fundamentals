import tester.*;

// Double dispatch approach to structural equality.

// Represents a geometric shape
interface Shape {
    // Is this shape the same as that shape?
    Boolean same(Shape that);

    // Is this shape the same as that square?
    Boolean sameSq(Sq that);
    // Is this shape the same as that circle?
    Boolean sameCirc(Circ that);
}

abstract class AShape implements Shape {
    public Boolean sameSq(Sq that) { return false; }
    public Boolean sameCirc(Circ that) { return false; }
}

// Represents a square
class Sq extends AShape {
    Integer size;
    Sq(Integer size) {
	this.size = size;
    }

    // Is this square the same as that shape?
    public Boolean same(Shape that) {
	return that.sameSq(this);
    }

    // Is this square the same as that square?
    public Boolean sameSq(Sq that) {
	return this.size.equals(that.size);
    }
}

// Represents a circle
class Circ extends AShape {
    Integer radius;
    Circ(Integer radius) {
	this.radius = radius;
    }

    // Is this circle the same as that shape?
    public Boolean same(Shape that) {
	return that.sameCirc(this);
    }

    // Is this circle the same as that circle?
    public Boolean sameCirc(Circ that) {
	return this.radius.equals(that.radius);
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
