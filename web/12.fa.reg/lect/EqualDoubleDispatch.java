/***********************************************
 *  CS2510 Spring 2012 
 *  Structural equality via double-dispatch
 ***********************************************/

import tester.*;

// Represents a Location
class Loc {
    Integer x;
    Integer y;
    
    Loc(Integer x, Integer y){
        this.x = x;
        this.y = y;
    }
    
    // Is this Loc the same as the given one
    Boolean sameLoc(Loc that) {
        return (that.x == this.x &&
                that.y == this.y);
    }
}


// Represents a Shape
interface IShape {
    // Is that shape the same as this Shape
    public Boolean sameShape(IShape that);

    // Is that Circle the same as this Shape
    public Boolean sameCirc(Circ that);
    // Is that Rect the same as this Shape
    public Boolean sameRect(Rect that);
    // Is that Combo the same as this Shape
    public Boolean sameCombo(Combo that);
}

// Represents a Circle
class Circ implements IShape {
    Loc loc;
    Integer rad;

    Circ(Loc loc, Integer rad) {
        this.loc = loc; this.rad = rad;
    }
    
    /** Template:
     *    Fields:
     *      ... this.loc ... -- Loc
     *      ... this.rad ... -- int
     *      
     *    Methods:
     *      ... this.sameShape(IShape) ... -- Boolean
     *      ... this.sameCirc(Circ) ... -- Boolean
     *      ... this.sameRect(Rect) ... -- Boolean
     *      ... this.sameCombo(Combo) ... -- Boolean
     *      
     *    Methods of Fields:
     *      ... this.loc.sameLoc() ... -- Boolean
     */

    // Is that Shape the same as this Circ
    public Boolean sameShape(IShape that) {
        return that.sameCirc(this);
    }
    // Is that Circle the same as this Circ
    public Boolean sameCirc(Circ that) {
        return (this.loc.sameLoc(that.loc) &&
                this.rad == that.rad);
    }
    // Is that Rect the same as this Circ
    public Boolean sameRect(Rect that) {
        return false;
    }
    // Is that Combo the same as this Circ
    public Boolean sameCombo(Combo that) {
        return false;
    }
}

// Represents a Rectangle
class Rect implements IShape {
    Loc loc;
    Integer w;
    Integer h;
    
    Rect(Loc loc, Integer w, Integer h) {
        this.loc = loc; this.w = w; this.h = h;
    }
    
    // Is that Shape the same as this Rect
    public Boolean sameShape(IShape that) {
        return that.sameRect(this);
    }
    // Is that Circ the same as this Rect
    public Boolean sameCirc(Circ that){
        return false;
    }
    // Is that Rect the same as this Rect
    public Boolean sameRect(Rect that){
        return (this.loc.sameLoc(that.loc) &&
                this.w == that.w &&
                this.h == that.h);
    }
    // Is that Combo the same as this Rect
    public Boolean sameCombo(Combo that){
        return false;
    }
}

// Represents a Combo
class Combo implements IShape {
    IShape top;
    IShape bot;
    
    Combo(IShape top, IShape bot) {
        this.top = top; this.bot = bot;
    }

    // Is that Shape the same as this Combo
    public Boolean sameShape(IShape that){
        return that.sameCombo(this);
    }
    // Is that Circ the same as this Combo
    public Boolean sameCirc(Circ that){
        return false;
    }
    // Is that Rect the same as this Combo
    public Boolean sameRect(Rect that){
        return false;
    }
    // Is that Combo the same as this Combo
    public Boolean sameCombo(Combo that){
        return (that.top.sameShape(this.top) &&
                that.bot.sameShape(this.bot));
    }
}

// Exhaustive Testing...
class LectureExamples{
    IShape c1 = new Circ(new Loc(50, 50), 10);
    IShape c2 = new Circ(new Loc(60, 40), 30);
    IShape c3 = new Circ(new Loc(50, 50), 10);
    IShape r1 = new Rect(new Loc(50, 50), 10, 20);
    IShape r2 = new Rect(new Loc(60, 40), 30, 50);
    IShape r3 = new Rect(new Loc(50, 50), 10, 20);
    IShape comb1 = new Combo(this.c1, this.c2);
    IShape comb2 = new Combo(this.r1, this.c1);
    IShape comb3 = new Combo(this.c1, this.c2);
    IShape comb4 = new Combo(this.comb1, this.comb2);
    IShape comb5 = new Combo(this.comb1, this.comb2);
    IShape comb6 = new Combo(this.comb1, this.comb1);

    // Test the same kinds of things (Circ/Circ, Rect/Rect)
    Boolean testSameOnes(Tester t){
        return (t.checkExpect(this.c1.sameShape(this.c1), true) &&
                t.checkExpect(this.c1.sameShape(this.c2), false) &&
                t.checkExpect(this.c3.sameShape(this.c1), true) &&
                t.checkExpect(this.r1.sameShape(this.r1), true) &&
                t.checkExpect(this.r1.sameShape(this.r2), false) &&
                t.checkExpect(this.r3.sameShape(this.r1), true) &&
                t.checkExpect(this.comb1.sameShape(this.comb1), true) &&
                t.checkExpect(this.comb1.sameShape(this.comb2), false) &&
                t.checkExpect(this.comb4.sameShape(this.comb5), true) &&
                t.checkExpect(this.comb5.sameShape(this.comb6), false));
    }
    // Test different kinds of things (Circ/Rect, Rect/Combo) all false
    Boolean testSameDiffs(Tester t){
        return (t.checkExpect(this.r1.sameShape(this.c1), false) &&
                t.checkExpect(this.comb1.sameShape(this.c2), false) &&
                t.checkExpect(this.c1.sameShape(this.r1), false) &&
                t.checkExpect(this.comb1.sameShape(this.r2), false) &&
                t.checkExpect(this.c1.sameShape(this.comb1), false) &&
                t.checkExpect(this.r2.sameShape(this.comb2), false));
    }
}
