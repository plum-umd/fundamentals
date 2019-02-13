// Authors: name
// Assignment 3

package edu.umd.cmsc132A;

import tester.Tester;

public class Assign3 {
    // Intentionally blank; leave blank
}

//-----------------------------------------------------------------------------
// Problem 1

// A Rocket is a new Rocket(Double)
// Interp: units of time that have passed since launch
class Rocket {
    Double n;

    Rocket(Double n) {
        this.n = n;
    }

    // Uniform acceleration of Rocket in pixels per unit^2 of time
    static Double A = 10.0;

    // Advance this given rocket by 1 unit of time
    public Rocket tick() {
        return new Rocket(this.n + 1);
    }

    // The displacement from the launch site of this rocket
    public Double displacement() {
        // Math.pow is Java's power function
        return 1 / 2 * this.A * Math.pow(this.n, 2);
    }
}

//-----------------------------------------------------------------------------
// Problem 2



//-----------------------------------------------------------------------------
// Problem 3



//-----------------------------------------------------------------------------
// Problem 4



//-----------------------------------------------------------------------------
// Problem 5



//-----------------------------------------------------------------------------
// Problem 6



//-----------------------------------------------------------------------------
// Tests

class Tests {
    Boolean testRocketTick(Tester t) {
        return t.checkExpect(new Rocket(5.0).tick(), new Rocket(6.0));
    }

    Boolean testRocketDisplacement(Tester t) {
        return t.checkExpect(new Rocket(0.0).displacement(), 0.0)
                && t.checkExpect(new Rocket(1.0).displacement(), 1 / 2 * Rocket.A)
                && t.checkExpect(new Rocket(5.0).displacement(), 1 / 2 * Rocket.A * 25.0);

    }
}