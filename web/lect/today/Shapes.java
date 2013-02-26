interface Shape {}

class Posn {
    Integer x;
    Integer y;
    Posn(Integer x, Integer y) {
	this.x = x;
	this.y = y;
    }
}

abstract class AShape {
    String color;
    AShape(String color) {
	this.color = color;
    }
}

class Rect extends AShape implements Shape {
    Posn center;
    Integer width;
    Integer height;
    Rect(String color, Posn center, Integer width, Integer Height) {
	super(color);
	this.center = center;
	this.width = width;
	this.height = height;
    }
}
