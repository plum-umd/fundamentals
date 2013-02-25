import tester.*;

// A Type is one of:
// - primitive type (int, bool, float, ...)
// - Class
// - Interface

interface Shape {
    // Put the given shape on top of this shape.
    Shape overlay(Shape s);
}

abstract class AShape implements Shape {
    // Put the given shape on top of this shape.
    public Shape overlay(Shape s) {
	return new Combo(s, this);
    }
}

class Rect extends AShape implements Shape {
    Integer w;
    Integer h;
    Rect(Integer w, Integer h) {
	this.w = w;
	this.h = h;
    }
}

class Circ extends AShape implements Shape {
    Integer r;
    Circ(Integer r) {
	this.r = r;
    }
}

class Combo extends AShape implements Shape {
    Shape top;
    Shape bot;
    Combo(Shape top, Shape bot) {
	this.top = top;
	this.bot = bot;
    }
}

class Examples {
    Rect r = new Rect(10, 20);

    RuntimeException e = new RuntimeException("Spring break WOOO");

    Boolean testOverlay(Tester t) {
	return t.checkExpect(r.overlay(r), new Combo(r, r));
    }
}