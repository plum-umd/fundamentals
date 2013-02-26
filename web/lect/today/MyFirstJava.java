import tester.Tester;

// Represents a point on the Cartesian plane
// A Posn is new Posn(x,y) where x and y are positive integers.
class Posn {
    Integer x;
    Integer y;

    Posn(Integer x, Integer y) {
	this.x = x;
	this.y = y;
    }

    Boolean isAtOrigin() {
	return this.x == 0
	    && this.y == 0;
    }

    Posn move() {
	return new Posn(this.x + 1,this.y + 1);
    }
}

// find : Integer Integer -> MaybePosn

// A MaybePosn is one of:
// - NotAPosn
// - Posn

interface MaybePosn {
    Posn get();
    Boolean isSome();
}

class Some implements MaybePosn {
    Posn p;
    Some(Posn p) {
	this.p = p;
    }

    public Boolean isSome() {
	return true;
    }

    public Posn get() {
	return this.p;
    }
}

class None implements MaybePosn {
    None() {}

    public Posn get() {
	throw new RuntimeException("None");
    }

    public Boolean isSome() {
	return false;
    }
}

interface LoP {
    // Find the first posn with given x, if there is one.
    public MaybePosn find(Integer x);

    // Find the first posn with given x, damnit.
    public Posn findDamnit(Integer x);

    public Posn anotherFind(Integer x);
}

abstract class ALoP implements LoP {
    public Posn findDamnit(Integer x) {
	return this.find(x).get();
    }
}

class Empty extends ALoP implements LoP {
    Empty() {}

    public MaybePosn find(Integer x) {
	return new None();
    }

    public Posn anotherFind(Integer x) {
	return null;
    }
}

class Cons extends ALoP implements LoP {
    Posn first;
    LoP rest;
    Cons(Posn first, LoP rest) {
	this.first = first;
	this.rest = rest;
    }

    public MaybePosn find(Integer x) {
	if (this.first.x == x) {
	    return new Some(this.first);
	} else {
	    return this.rest.find(x);
	}
    }

    public Posn anotherFind(Integer x) {
	return null;
    }

}




class Examples {
    void testPosn(Tester t) {
	t.checkExpect(new Posn(3, 4).x, 3);
	// (check-expect ((new posn% 3 4) . is-at-origin?)
	t.checkExpect(new Posn(3, 4).isAtOrigin(), false);
    }

    void testFind(Tester t) {
	LoP ls = new Cons(new Posn(3, 4), new Empty());
	t.checkExpect(ls.find(3),
		      new Some(new Posn(3, 4)));
	t.checkExpect(ls.find(5),
		      new None());
	t.checkExpect(ls.findDamnit(3),
		      new Posn(3, 4));

	t.checkExpect((ls.anotherFind(3) instanceof Posn), true);
    }
}

// ;; A Posn is a (new posn% Integer Integer)
// (define-class posn%
//   (fields x y))

/* Much less fun
   than the #|
*/