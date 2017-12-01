interface Pred<T> {
    // Does this predicate hold on the given posn?
    Boolean pred(T p);
}

class SameXandY implements Pred<Posn> {
    // Does the given posn have the same x and y field?
    Boolean pred(Posn p) {
	return p.x.equals(p.y);
    }
}

class Posn {
    Integer x;
    Integer y;
    Posn(Integer x, Integer y) {
	if (x < 0) {
	    throw new RuntimeException("x was negative.");
	} else {
	    this.x = x;
	    this.y = y;
	}
    }
}

class ColorPosn extends Posn {
    String c;
    ColorPosn(Integer x, Integer y, String c) {
	super(x, y);
	this.c = c;
    }

    ColorPosn(Integer x, Integer y, Boolean b) {
	this(x, y, b ? "Transparent" : "Red");
    }
}
