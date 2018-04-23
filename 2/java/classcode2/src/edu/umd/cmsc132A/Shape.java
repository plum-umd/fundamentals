package edu.umd.cmsc132A;

// Equality:
// - reflexive  x = x
// - symmetric  x = y => y = x
// - transitive x = y, y = z => x = z
// - total      gives an answer for all values of this type


interface Shape {
    // Is this shape "equal" to the given one?
    Boolean sameShape(Shape s);

    // Is this shape equal to the given rectangle?
    Boolean sameRect(Rect r);

    // Is this shape equal to the given circle?
    Boolean sameCirc(Circ c);

    // Is this shape equal to the given square?
    Boolean sameSqr(Sqr s);

    // Is this shape equal to the given ellipse?
    Boolean sameEllipse(Ellipse e);

    // Is this shape equal to the given combo?
    Boolean sameCombo(Combo c);

    <R> R accept(ShapeVisitor<R> v);
}

interface ShapeVisitor<R> {
    R visitRect(Rect r);
    R visitCirc(Circ r);
    R visitEllipse(Ellipse e);
    R visitCombo(Combo c);
}

// Compute area of a shape
class ShapeAreaVisitor implements ShapeVisitor<Double> {

    public Double visitRect(Rect r) {
        return r.w * r.h * 1.0;
    }

    public Double visitCirc(Circ c) {
        return c.r * c.r * Math.PI;
    }

    public Double visitEllipse(Ellipse e) {
        return Math.PI * e.w * e.h;
    }

    public Double visitCombo(Combo c) {
        return c.s1.accept(this) + c.s2.accept(this);
    }

}


abstract class AShape implements Shape {

    public Boolean sameRect(Rect r) {
        return false;
    }

    public Boolean sameSqr(Sqr s) {
        return false;
    }

    public Boolean sameCirc(Circ c) {
        return false;
    }

    public Boolean sameEllipse(Ellipse e) {
        return false;
    }

    public Boolean sameCombo(Combo c) {
        return false;
    }
}

abstract class AShapePt extends AShape {
    Integer x, y;

    AShapePt(Integer x, Integer y) {
        this.x = x;
        this.y = y;
    }
}

class Rect extends AShapePt {
    Integer w, h;

    Rect(Integer x, Integer y, Integer w, Integer h) {
        super(x, y);
        this.w = w;
        this.h = h;
    }

    public Boolean sameShape(Shape s) {
        return s.sameRect(this);
    }

    public Boolean sameRect(Rect r) {
        return this.x.equals(r.x) &&
                this.y.equals(r.y) &&
                this.w.equals(r.w) &&
                this.h.equals(r.h);
    }

    public <R> R accept(ShapeVisitor<R> v) {
        return v.visitRect(this);
    }
}

class Sqr extends Rect {

    Sqr(Integer x, Integer y, Integer s) {
        super(x, y, s, s);
    }

    public Boolean sameShape(Shape s) {
        return s.sameSqr(this);
    }

    public Boolean sameSqr(Sqr s) {
        return this.x.equals(s.x) &&
                this.y.equals(s.y) &&
                this.w.equals(s.w) &&
                this.h.equals(s.h);
    }

    public Boolean sameRect(Rect r) {
        return false;
    }
}

class Circ extends AShapePt {
    Integer r;

    Circ(Integer x, Integer y, Integer r) {
        super(x, y);
        this.r = r;
    }

    public Boolean sameShape(Shape s) {
        return s.sameCirc(this);
    }

    public Boolean sameCirc(Circ c) {
        return this.x.equals(c.x) &&
                this.y.equals(c.y) &&
                this.r.equals(c.r);
    }

    public <R> R accept(ShapeVisitor<R> v) {
        return v.visitCirc(this);
    }
}

class Ellipse extends AShapePt {
    Integer w, h;

    Ellipse(Integer x, Integer y, Integer w, Integer h) {
        super(x, y);
        this.w = w;
        this.h = h;
    }

    public Boolean sameShape(Shape s) {
        return s.sameEllipse(this);
    }

    public Boolean sameEllipse(Ellipse e) {
        return false; // STUB
    }

    public <R> R accept(ShapeVisitor<R> v) {
        return v.visitEllipse(this);
    }
}


class Combo extends AShape {
    Shape s1;
    Shape s2;

    Combo(Shape s1, Shape s2) {
        this.s1 = s1;
        this.s2 = s2;
    }

    public Boolean sameShape(Shape s) {
        return s.sameCombo(this);
    }

    public Boolean sameCombo(Combo c) {
        return this.s1.sameShape(c.s1) &&
                this.s2.sameShape(c.s2);
    }

    public <R> R accept(ShapeVisitor<R> v) {
        return v.visitCombo(this);
    }
}



