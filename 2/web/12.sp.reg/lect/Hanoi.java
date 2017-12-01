class Hanoi {
    Peg a;
    Peg b;
    Peg c;
    Hanoi(Integer n) {
	this.a = ;
	this.b = ;
	this.c = ;
    }
}

interface Peg {
    Peg makePeg(Integer n);
}

class MTPeg {}

class ConsPeg {
    Integer first;
    Peg rest;
    ConsPegt(Integer first, Peg rest) {
	this.first = first;
	this.rest = rest;
    }
}

