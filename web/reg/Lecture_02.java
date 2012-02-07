/************************************
 *  CS2510 Fall 2011 
 *  Lecture #2
 *  Data Definitons using Java
 ************************************/

// This is a comment

/*
 * +--------------------+
 * |      Rocket        |
 * +--------------------+
 * | String  name       |
 * | Posn    pos        |
 * | boolean landingHuh |
 * +--------------------+
 */

// A Pizza is: (make-pizza String Boolean String Number)
// (define-struct pizza (cheese meat? sauce size))

public class Pizza {
    //Fields
    String cheese;
    boolean meatHuh;
    String sauce;
    int size;
    
    // Constructor
    Pizza(String cheese, boolean meatHuh, String sauce, int size) {
        this.cheese = cheese;
        this.meatHuh = meatHuh;
        this.sauce = sauce;
        this.size = size;
    }
}

// Examples for Pizza and simple data
class PizzaExamples {
    Pizza hawaiian = new Pizza("Mozz", true, "Vodka", 42);
    Pizza plain = new Pizza("Gouda", false, "Tomato", 12);
    Pizza buffalo = new Pizza("Blue", true, "Buffalo/Vodka", 16);
   
    
    double aDouble = 45.34;
    char theCharA = 'A';
    String hCheese = this.hawaiian.cheese;
}

// Represents a Position in 2D Space
class Posn {
    int x;
    int y;
    
    Posn(int xcoord, int ycoord) {
        this.x = xcoord;
        this.y = ycoord;
    }
}


//Russian Doll interface
interface RD {
}

// A solid innermost doll
class SolidDoll implements RD {
    SolidDoll() {}
}

// A fillable doll
class Doll implements RD {

    // The doll contained inside this doll
    RD inside;

    Doll(RD inside) {
        this.inside = inside;
    }
    
}


class LectureExamples {
    LectureExamples() {

        Posn thisisaposn = new Posn(1,2);
        int px = thisisaposn.x;

        // A SolidDoll (make-soliddoll)
        // A Doll is: (make-doll RD)
        // An RD is either a SolidDoll or Doll
        //(make-doll (make-doll (make-doll (make-soliddoll))))
        Doll d4 = new Doll(new Doll(new Doll(new SolidDoll())));
        SolidDoll sd = new SolidDoll();
        Doll d1 = new Doll(sd);
        Doll d2 = new Doll(d1);
        Doll d3 = new Doll(d2);
    }
}