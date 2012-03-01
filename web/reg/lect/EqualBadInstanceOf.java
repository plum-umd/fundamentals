/***********************************************
 *  CS2510 Spring 2012 
 *  Broken structural equality via instanceof
 ***********************************************/

import tester.*;

interface Drink {
    // Is this drink the same as that drink?
    Boolean same(Drink that);
}

class Tea implements Drink {
    // Is this tea the same as that drink?
    public Boolean same(Drink that) {
	return (that instanceof Tea);
    }
}

class Coffee implements Drink {
    // Is this coffee the same as that drink?
    public Boolean same(Drink that) {
	return (that instanceof Coffee);
    }
}

class PassingExamples {
    Drink t = new Tea();
    Drink c = new Coffee();
    
    public Boolean testSameDrink(Tester t) {
	return t.checkExpect(this.t.same(this.t), true)
	    && t.checkExpect(this.c.same(this.c), true)
	    && t.checkExpect(this.t.same(this.c), false)
	    && t.checkExpect(this.c.same(this.t), false);
    }
}

// This all seems to work just fine, but what if we extend our program
// as follows:

class Latte extends Coffee {
    // We'll even go to the trouble of overriding same to make sure
    // Lattes are different from coffees:
    public Boolean same(Drink that) {
	return (that instanceof Latte);
    }
}

// BUT NOW OUR PROGRAM IS BROKEN
// and we can write a test case to show it.

class FailingExamples {
    Drink c = new Coffee();
    Drink l = new Latte();

    public Boolean testSameCoffee(Tester t) {
	return t.checkExpect(this.l.same(this.c), false)  // works!
	    && t.checkExpect(this.c.same(this.l), false); // doesn't!
    }
}

// We can "fix" the problem by changing Coffee to take into account
// the fact that Lattes exist now, but 
// ******* THAT IS NOT OBJECT-ORIENTED PROGRAMMING ***********

// Real object-oriented programming allows for Coffee to be extended
// and to work properly WITHOUT changes to its definition.  See the
// other file for how to work this problem out correctly in an
// OO-style.
