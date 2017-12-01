import tester.*;

// Represents a complex number
class Complex {
    Double real;
    Double imag;
    Complex(Double real, Double imag) {
	this.real = real;
	this.imag = imag;
    }
}

// Represents a shape
interface Shape {}

// Represents a position in Cartesian coordinate system
class Posn {
    Integer x;
    Integer y;
    Posn(Integer x, Integer y) {
	this.x = x;
	this.y = y;
    }
}

class Point implements Shape {
    Posn pinhole;
    Point(Posn pinhole) {
	this.pinhole = pinhole;
    }
}

class Rectangle implements Shape {
    Posn pinhole;
    Integer width;
    Integer height;
    Rectangle(Posn pinhole, Integer width, Integer height) {
	this.pinhole = pinhole;
	this.width = width;
	this.height = height;
    }
}

class Circle implements Shape {
    Posn pinhole;
    Integer radius;
    Circle(Posn pinhole, Integer radius) {
	this.pinhole = pinhole;
	this.radius = radius;
    }
}

// LoI: See solution 2.1

// Represents a list of complex numbers
interface LoC {}

// Represents an empty list of complex numbers
class MTC implements LoC {
    MTC() {}
}

// Represents a non-empty list of complex numbers
class ConsC implements LoC {
    Complex first;
    LoC rest;
    ConsC(Complex first, LoC rest) {
	this.first = first;
	this.rest = rest;
    }
}

class Examples {
    Examples() {}
    Complex i = new Complex(0.,1.);
    Complex j = new Complex(4.,3.);
    Shape circ = new Circle(new Posn(3,4), 9);
    Shape rect = new Rectangle(new Posn(3,4), 8, 10);
    Shape orig = new Point(new Posn(0,0));
    LoC mt = new MTC();
    LoC ij = new ConsC(i, new ConsC(j, mt));
}