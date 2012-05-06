/***********************************************
 *  CS2510 Spring 2012 
 *  Coffee, done right
 ***********************************************/

import tester.*;

interface Drink {
    // Is this drink the same as that drink?
    Boolean same(Drink that);

    // Is this drink the same as that tea?
    Boolean sameTea(Tea that);
    // Is this drink the same as that coffee?
    Boolean sameCoffee(Coffee that);
    // Is this drink the same as that latte?
    Boolean sameLatte(Latte latte);
}

abstract class ADrink {
    public Boolean sameTea(Tea that) { return false; }
    public Boolean sameCoffee(Coffee that) { return false; }
    public Boolean sameLatte(Latte that) { return false; }
}

class Tea extends ADrink implements Drink {
    // Is this tea the same as that drink?
    public Boolean same(Drink that) {
	return that.sameTea(this);
    }

    public Boolean sameTea(Tea that) {
	return true;
    }
}

class Coffee extends ADrink implements Drink {
    // Is this coffee the same as that drink?
    public Boolean same(Drink that) {
	return that.sameCoffee(this);
    }

    public Boolean sameCoffee(Coffee that) { 
	return true;
    }
}

class Latte extends Coffee {
    public Boolean same(Drink that) {
	return that.sameLatte(this);
    }
    
    public Boolean sameLatte(Latte that) {
	return true;
    }

    public Boolean sameCoffee(Coffee that) {
	return false;
    }
}

class Examples {
    Drink t = new Tea();
    Drink c = new Coffee();
    Drink l = new Latte();
    
    public Boolean testSameDrink(Tester t) {
	return t.checkExpect(this.t.same(this.t), true)
	    && t.checkExpect(this.c.same(this.c), true)
	    && t.checkExpect(this.t.same(this.c), false)
	    && t.checkExpect(this.c.same(this.t), false);
    }
    public Boolean testSameCoffee(Tester t) {
	return t.checkExpect(this.l.same(this.c), false)  // works!
	    && t.checkExpect(this.c.same(this.l), false); // still works!
    }

}

