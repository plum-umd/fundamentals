package edu.umd.cmsc132A;

import tester.*;

public class Lab11 {}

class Capulet {
    String name;
    Capulet(String nm) { this.name = nm; }
    String greet(Montague m) {
        return name + ": I bite my thumb at you!";
    }

    String greet(Romeo m) {
        return name + ": I bite my thumb at you, Romeo!";
    }
}

class Juliet extends Capulet {
    Juliet() { super("Juliet"); }

    String greet(Romeo m) {
        return name + ": Wherefore art thou, Romeo?";
    }
}

class Montague {}

class Romeo extends Montague {}

class Main {
    Capulet sampson = new Capulet("Sampson");
    Montague abram = new Montague();
    Juliet juliet = new Juliet();
    Romeo romeo = new Romeo();

    Boolean testGreeting(Tester t) {
        return false;
    }

    Capulet hoodedJuliet = juliet;
    Montague hoodedRomeo = romeo;

    Boolean testGreetingHooded(Tester t) {
        return false;
    }

    Boolean testBeGreeted(Tester t) {
        return false;
    }
}