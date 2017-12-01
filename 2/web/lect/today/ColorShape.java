import tester.*;
/* This program demonstrates a simple use of abstract classes to
   abstract identical data and implementation obligations.
*/
/*
          +---------+
          |  Shape  |
          +---------+
              /_\
               |
        +--------------+
        |    AShape    |
        +--------------+     +-----------+
        | Posn center  |---->|    Posn   |
        | String color |     +-----------+
        +--------------+     | Integer x |
         /_\          /_\    | Integer y |
          |            |     +-----------+
          |            |
 +----------------+  +-----------------+
 |      Rect      |  |     ACircle     |
 +----------------+  +-----------------+
 | Integer width  |  | Integer radius  |
 | Integer height |  +-----------------+
 +----------------+    /_\   /_\
                        |     |
                      Circle  GreenCircle
                       ...     ...
 */

// Represents a shape
interface Shape {
    // -----------------------------------------------
    // Structural equality via double dispatch

    // Is this shape the same as that shape?
    Boolean sameAs(Shape that);
    // Is this shape the same as that rectangle?
    Boolean sameAsRect(Rect that);
    // Is this shape the same as that circle?
    Boolean sameAsCircle(Circle that);

    // -----------------------------------------------
    // Structural equality via safe casting

    // Is this shape the same as that shape?
    Boolean ekwal(Shape that);
    // Is this shape a rectangle?
    Boolean isRect();
    // Is this shape a circle?
    Boolean isCircle();
    // Convert this shape to a circle
    Circle toCircle();
    // Convert this shape to a rectangle
    Rect toRect();
}

// Represents a Cartesian coordinate
class Posn {
    Integer x;
    Integer y;
    Posn(Integer x, Integer y) {
	this.x = x;
	this.y = y;
    }

    // Is this posn the same as that posn?
    public Boolean sameAs(Posn that) {
	return this.x == that.x
	    && this.y == that.y;
    }
}

// Every shape has a color and a center point.
abstract class AShape implements Shape {
    String color;
    Posn center;
    AShape(String color, Posn center) {
	this.color = color;
	this.center = center;
    }

    // -----------------------------------------------
    // Structural equality via double dispatch
    public Boolean sameAsCircle(Circle that) {
	return false;
    }
    public Boolean sameAsRect(Rect that) {
	return false;
    }

    // -----------------------------------------------
    // Structural equality via safe casting
    public Boolean isRect() {
	return false;
    }
    public Boolean isCircle() {
	return false;
    }
    public Circle toCircle() {
	throw new RuntimeException("Circle ain't no Rectangle!");
    }
    public Rect toRect() {
	throw new RuntimeException("Circle ain't no Rectangle!");
    }
}

// Represents a rectangle
class Rect extends AShape {
    Integer width;
    Integer height;
    // Inherits the color and center fields
    Rect(String color, Posn center, Integer width, Integer height) {
	super(color, center); // Call the super class constructor
	this.width = width;
	this.height = height;
    }

    // -----------------------------------------------
    // Structural equality for Rectangle variant

    // Is this rectangle the same as that rectangle?
    public Boolean sameAsRect(Rect that) {
	return this.color.equals(that.color)
	    && this.width.equals(that.width)
	    && this.height.equals(that.height)
	    && this.center.sameAs(that.center);
    }

    // -----------------------------------------------
    // Structural equality via double dispatch
    // Is this rectangle the same as that shape?
    public Boolean sameAs(Shape that) {
	return that.sameAsRect(this);
    }

    // -----------------------------------------------
    // Structural equality via safe casting
    // Is this rectangle the same as that shape?
    public Boolean ekwal(Shape that) {
	if (that.isRect()) {
	    return this.sameAsRect(that.toRect());
	} else {
	    return false;
	}
    }

    // Is this rectangle a rectangle?
    public Boolean isRect() {
	return true;
    }

    // Convert this rectangle to a rectangle
    public Rect toRect() {
	return this;
    }
}

class Circle extends AShape {
    Integer radius;
    Circle(String color, Posn center, Integer radius) {
	super(color, center);
	this.radius = radius;
    }

    // -----------------------------------------------
    // Structural equality for Circle variant

    // Is this circle the same as that circle?
    public Boolean sameAsCircle(Circle that) {
	return this.radius.equals(that.radius)
	    && this.color.equals(that.color)
	    && this.center.sameAs(that.center);
    }

    // -----------------------------------------------
    // Structural equality via double dispatch

    // Is this circle the same as that shape?
    public Boolean sameAs(Shape that) {
	return that.sameAsCircle(this);
    }

    // Is this circle a circle?
    public Boolean isCircle() {
	return true;
    }

    // -----------------------------------------------
    // Structural equality via safe casting

    // Is this circle the same as that shape?
    public Boolean ekwal(Shape that) {
	if (that.isCircle()) {
	    return this.sameAsCircle(that.toCircle());
	} else {
	    return false;
	}
    }

    // Convert this circle into a circle
    public Circle toCircle() {
	return this;
    }
}

class Examples {
    void testSameAsPosn(Tester t) {
	Posn p = new Posn(5, 7);
	t.checkExpect(p.sameAs(p), true);
	t.checkExpect(new Posn(3, 4).sameAs(new Posn(3, 4)), true);
	t.checkExpect(p.sameAs(new Posn(3, 4)), false);
    }

    void testSameAsShape(Tester t) {
	Posn p1 = new Posn(5, 7);
	Posn p2 = new Posn(7, 5);
	t.checkExpect(        new Rect("blue", p1, 100, 50)
		      .sameAs(new Rect("blue", p1, 100, 50)),
			      true);
	t.checkExpect(        new Rect("blue", p1, 100, 50)
		      .sameAs(new Rect("blue", p2, 100, 50)),
			      false);
	t.checkExpect(        new Rect("blue", p1, 100, 50)
		      .sameAs(new Rect("blue", p1, 50, 100)),
			      false);
	t.checkExpect(        new Rect("blue", p1, 100, 50)
		      .sameAs(new Rect("cyan", p1, 100, 50)),
			      false);

	t.checkExpect(        new Circle("blue", p1, 50)
		      .sameAs(new Circle("blue", p1, 50)),
			      true);
	t.checkExpect(        new Circle("blue", p1, 50)
		      .sameAs(new Circle("blue", p2, 50)),
			      false);
	t.checkExpect(        new Circle("blue", p1, 50)
		      .sameAs(new Circle("blue", p1, 100)),
			      false);
	t.checkExpect(        new Circle("blue", p1, 50)
		      .sameAs(new Circle("cyan", p1, 50)),
			      false);

	t.checkExpect(        new Circle("blue", p1, 50)
		      .sameAs(new Rect("blue", p1, 50, 50)),
			      false);
	t.checkExpect(        new Rect("blue", p1, 50, 50)
		      .sameAs(new Circle("blue", p1, 50)),
			      false);
    }

    void testEkwal(Tester t) {
	Posn p1 = new Posn(5, 7);
	Posn p2 = new Posn(7, 5);
	t.checkExpect(       new Rect("blue", p1, 100, 50)
		      .ekwal(new Rect("blue", p1, 100, 50)),
			     true);
	t.checkExpect(       new Rect("blue", p1, 100, 50)
		      .ekwal(new Rect("blue", p2, 100, 50)),
			     false);
	t.checkExpect(       new Rect("blue", p1, 100, 50)
		      .ekwal(new Rect("blue", p1, 50, 100)),
			     false);
	t.checkExpect(       new Rect("blue", p1, 100, 50)
		      .ekwal(new Rect("cyan", p1, 100, 50)),
			     false);

	t.checkExpect(       new Circle("blue", p1, 50)
		      .ekwal(new Circle("blue", p1, 50)),
			     true);
	t.checkExpect(       new Circle("blue", p1, 50)
		      .ekwal(new Circle("blue", p2, 50)),
			     false);
	t.checkExpect(       new Circle("blue", p1, 50)
		      .ekwal(new Circle("blue", p1, 100)),
			     false);
	t.checkExpect(       new Circle("blue", p1, 50)
		      .ekwal(new Circle("cyan", p1, 50)),
			     false);

	t.checkExpect(       new Circle("blue", p1, 50)
		      .ekwal(new Rect("blue", p1, 50, 50)),
			     false);
	t.checkExpect(       new Rect("blue", p1, 50, 50)
		      .ekwal(new Circle("blue", p1, 50)),
			     false);
    }

}
