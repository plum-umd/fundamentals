import tester.*;

// to represent a geometric shape
interface IShape {
    public <Fred> Fred accept(IShapeVisitor<Fred> v);
}

// Represents a Double computation over shapes
interface IShapeVisitor<T> {
    T visitCircle(Integer x, Integer y, Integer rad);
    T visitRect(Integer x, Integer y, Integer w, Integer h);
    T visitCombo(IShape top, IShape bot);
}

class ShapeArea implements IShapeVisitor<Double> {
    public Double visitCircle(Integer x, Integer y, Integer rad) {
	return Math.pow(rad, 2) * Math.PI;
    }

    public Double visitRect(Integer x, Integer y, Integer w, Integer h) {
	return w * h * 1.0;
    }

    public Double visitCombo(IShape top, IShape bot) {
	return top.accept(this) + bot.accept(this);
    }
}

// to represent a circle
class Circle implements IShape {
    Integer x;
    Integer y;
    Integer rad;
	
    Circle(Integer x, Integer y, Integer rad) {
	this.x = x;
	this.y = y;
	this.rad = rad;
    }

    public <Fred> Fred accept(IShapeVisitor<Fred> v) {
	return v.visitCircle(this.x, this.y, this.rad);
    }
}

// to represent a rectangle
class Rect implements IShape {
    Integer x;
    Integer y;
    Integer w;
    Integer h;
	
    Rect(Integer x, Integer y, Integer w, Integer h) {
	this.x = x;
	this.y = y;
	this.w = w;
	this.h = h;
    }

    public <Fred> Fred accept(IShapeVisitor<Fred> v) {
	return v.visitRect(this.x, this.y, this.w, this.h);
    }
}

// to represent a combined shape
class Combo implements IShape {
    IShape top;
    IShape bot;
	
    Combo(IShape top, IShape bot) {
	this.top = top;
	this.bot = bot;
    }

    public <Fred> Fred accept(IShapeVisitor<Fred> v) {
	return v.visitCombo(this.top, this.bot);
    }
}

class ExamplesShapes{
    ExamplesShapes() {}
	
    IShape circle = new Circle(50, 50, 50);
    IShape rleft = new Rect(20, 20, 20, 20);
    IShape rBot = new Rect(20, 60, 60, 20);
	
    IShape addMouth = new Combo(this.rBot, this.circle);
    IShape addLeftEye = new Combo(this.rleft, this.addMouth);
    IShape face = new Combo(new Rect(60, 20, 20, 20), this.addLeftEye);	

    IShapeVisitor<Double> area = new ShapeArea();

    Boolean testArea(Tester t) {
	return t.checkExpect(this.rBot.accept(this.area), 20*60.0);
    }
}