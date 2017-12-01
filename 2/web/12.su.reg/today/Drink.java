import tester.*;

// An Example showing the evil of instanceof and casts.
// To fix this problem: don't use instanceof and casts,
// use either
// - safe casting
// - double dispatch
// - visitors

// Represents a drink
interface Drink {
    // Is this drink the same as that drink?
    Boolean same(Drink that);
}

// Represents a cup of coffee.
class Coffee implements Drink {
    Integer sugars;
    Coffee(Integer sugars) {
	this.sugars = sugars;
    }

    // Is this coffee the same as that drink?
    public Boolean same(Drink that) {
	return (that instanceof Coffee)
	    && ((Coffee)that).sugars.equals(this.sugars);
    }
}

// Represents a cup of tea.
class Tea implements Drink {
    Tea() {}

    // Is this tea the same as that drink?
    public Boolean same(Drink that) {
	return (that instanceof Tea);
    }
}

// Represents a cup of coffee with Bailey's in it.
class Baileys extends Coffee {
    Baileys(Integer sugar) { super(sugar); }

    // Is this coffee w/ Bailey's the same as that drink?
    public Boolean same(Drink that) {
	return (that instanceof Baileys)
	    && ((Baileys)that).sugars.equals(this.sugars);
    }
}

class Examples {
    Examples() {}

    void testSame(Tester t) {
	t.checkExpect(new Baileys(0).same(new Coffee(0)), false);
	// This test fails because our program is BROKEN.
	t.checkExpect(new Coffee(0).same(new Baileys(0)), false);
    }
}
