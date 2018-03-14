package edu.umd.cmsc132A;

import tester.*;

public class Lab11 {}


class Capulet {
    String name;
    Capulet(String nm) { this.name = nm; }
    String greet(Montague m) {
        return name + ": I bite my thumb at you.";
    }

    String greet(Romeo m) {
        return name + ": I bite my thumb at you, Romeo.";
    }
}

class Juliet extends Capulet {
    Juliet() { super("Juliet"); }

    String greet(Romeo m) {
        return name + ": Wherefore art thou, Romeo?";
    }
}

class Montague {
    String beGreeted(Capulet c){
        return c.greet(this);
    }
}

class Romeo extends Montague {
    String beGreeted(Capulet c){
        return c.greet(this);
    }
}

class Main {
    Capulet sampson = new Capulet("Sampson");
    Montague abram = new Montague();
    Juliet juliet = new Juliet();
    Romeo romeo = new Romeo();

    Boolean testGreeting(Tester t) {
        return t.checkExpect(sampson.greet(abram), "Sampson: I bite my thumb at you.")
                && t.checkExpect(sampson.greet(romeo), "Sampson: I bite my thumb at you, Romeo.")
                && t.checkExpect(juliet.greet(abram), "Juliet: I bite my thumb at you.")
                && t.checkExpect(juliet.greet(romeo), "Juliet: Wherefore art thou, Romeo?");
    }

    Capulet hoodedJuliet = new Juliet();
    Montague hoodedRomeo = new Romeo();

    Boolean testGreetingHidden(Tester t) {
        return t.checkExpect(hoodedJuliet.greet(romeo), "Juliet: Wherefore art thou, Romeo?")
                && t.checkExpect(hoodedJuliet.greet(hoodedRomeo), "Juliet: I bite my thumb at you.")
                && t.checkExpect(juliet.greet(hoodedRomeo), "Juliet: I bite my thumb at you.");
    }


    // Montagues being greeted by Capulets

    Boolean testBeGreeted(Tester t) {
        return t.checkExpect(abram.beGreeted(sampson), "Sampson: I bite my thumb at you.")
                && t.checkExpect(romeo.beGreeted(sampson), "Sampson: I bite my thumb at you, Romeo.")
                && t.checkExpect(abram.beGreeted(juliet), "Juliet: I bite my thumb at you.")
                && t.checkExpect(romeo.beGreeted(juliet), "Juliet: Wherefore art thou, Romeo?");
    }

    Boolean testBeGreetedHidden(Tester t) {
        return t.checkExpect(romeo.beGreeted(hoodedJuliet), "Juliet: Wherefore art thou, Romeo?")
                && t.checkExpect(hoodedRomeo.beGreeted(hoodedJuliet), "Juliet: Wherefore art thou, Romeo?");
    }
}