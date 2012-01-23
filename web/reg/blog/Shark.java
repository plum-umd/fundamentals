import tester.*;

// An Ocean is a new Ocean(Shark, School)
// Interp: shark swimming near school of fish.
class Ocean {
    Shark shark;
    School school;
    Ocean(Shark shark, School school) {
	this.shark = shark;
	this.school = school;
    }    
}

// A Shark is a new Shark(Integer, Integer)
// Interp: new Shark(y) represents shark at height y in graphics coordinates
class Shark {
    Integer y;
    Integer health;
    Shark(Integer y, Integer health) {
	this.y = y;
	this.health = health;
    }

    // This shark chomps the fish
    public Shark chomp(Fish f) {
	// this.y this.health f.p f.health
	return new Shark(this.y, this.health + f.health);
    }	
}

// A Fish is a new Fish(Posn)
// Interp: new Fish(p) represents a fish located as position p
class Fish {
    Posn p;
    Integer health;
    Fish(Posn p, Integer health) {
	this.p = p;
	this.health = health;
    }

    // Move this fish left.
    public Fish moveLeft() {
	return new Fish(this.p.moveLeft(1), this.health);
    }

    // Is this fish close to that fish?
    /*
    public Boolean closeByHuh(Fish that) {
	this.p 
	this.health 
	that.p
	that.health
    }
    */

}

// A Posn is a new Posn(Integer, Integer)
// Interp: new Posn(x,y) represents (x,y) in graphics coordinates
class Posn {
    Integer x;
    Integer y;
    Posn(Integer x, Integer y) {
	this.x = x;
	this.y = y;
    }

    // Move this posn left i units.
    public Posn moveLeft(Integer i) {
	return new Posn(this.x-i, this.y);
    }
}

// A School is one of:
// - new EmptySchool()
// - new ConsSchool(Fish, School)

// Interp: a school of (an arbitrary number of) fish.
interface School {}

// Interp: a school with no fish
class EmptySchool implements School {
    EmptySchool() {}
}

// Interp: a school with at least one fish
class ConsSchool implements School {
    Fish first;
    School rest;    
    ConsSchool (Fish first, School rest) {
	this.first = first;
	this.rest = rest;
    }
}

class Examples {
    Examples() {}

    Posn o = new Posn(0, 0);
    Fish f = new Fish(o, 5);

    public boolean testPosnMoveLeft(Tester t) {
	return t.checkExpect(new Posn(0,0).moveLeft(5), new Posn(-5,0));
    }
}
    


