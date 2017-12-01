import tester.*;

class Posn {
    Integer x;
    Integer y;
    
    Posn(Integer x, Integer y) {
	this.x = x;
	this.y = y;
    }

    // Swap the x and y of this posn.
    // Old skool: swap : -> Posn
    Posn swap() {
	return new Posn(this.y, this.x);
    }

    // Move this posn by given change in x and y.
    // move : Integer Integer -> Posn
    Posn move(Integer dx, Integer dy) {
	return new Posn(this.x + dx, this.y + dy);
    }
}

class Examples {
    
    Boolean testPosn(Tester t) {
	return t.checkExpect(new Posn(3, 4), new Posn(3, 4))
	    && t.checkExpect(new Posn(3, 4).swap(), new Posn(4, 3))
	    && t.checkExpect(new Posn(3, 4).move(1, 2), new Posn(4, 6));
    }
    
}