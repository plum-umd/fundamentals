import tester.*;

interface Drink {
    // Is given drink same as this one?
    Boolean sameDrink(Drink d);
    Boolean sameTea(Tea t);
    Boolean sameCoffee(Coffee c);
}

abstract class ADrink implements Drink {
    public Boolean sameCoffee(Coffee c) {
	return false;
    }
    public Boolean sameTea(Tea t) {
	return false;
    }
}

class Tea extends ADrink implements Drink {
    String flavor;
    Tea(String flavor) {
	this.flavor = flavor;
    }  

    public Boolean sameDrink(Drink d) {
	if (d instanceof Tea) {
	    return ((Tea)d).flavor.equals(this.flavor);
	} else {
	    return false;
	}
	// return d.sameTea(this);
    }

    public Boolean sameTea(Tea t) {
	return t.flavor.equals(this.flavor);
    }

    /*
    public Boolean sameDrink(Drink d) {
	if (d.isTea()) {
	    return d.toTea().flavor.equals(this.flavor);
	} else {
	    return false;
	}
    }
    */
}

class Coffee extends ADrink implements Drink {
    Boolean cream;
    Integer sugar;
    Coffee(Boolean cream, Integer sugar) {
	this.cream = cream;
	this.sugar = sugar;
    }

    Coffee() {
	this.cream = true;
	this.sugar = 18;
    }

    Coffee(Integer sugar) {
	this.cream = false;
	this.sugar = sugar;
    }

    public Boolean sameDrink(Drink d) {
	if (d instanceof Coffee) {
	    return ((Coffee)d).sameCoffee(this);
	} else {
	    return false;
	}

	// return d.sameCoffee(this);
    }

    public Boolean sameCoffee(Coffee c) {
	return c.cream.equals(this.cream)
	    && c.sugar.equals(this.sugar);
    }
}

class Baileys extends Coffee {
    Integer oz;
    Baileys(Boolean cream, Integer sugar, Integer oz) {
	super(cream, sugar);
	this.oz = oz;
    }

    public Boolean sameDrink(Drink d) {
	if (d instanceof Baileys) {
	    return true;
	} else {
	    return false;
	}
    }
}

class Examples {
    Drink tea = new Tea("Gross");
    Drink coffee = new Coffee(true, 18); // DD
    Drink baileys = new Baileys(true, 18, 50);

    Boolean testSame(Tester t) {
	return t.checkExpect(tea.sameDrink(tea), true);
    }

    Boolean testNotSame(Tester t) {
	return t.checkExpect(tea.sameDrink(tea), true)
	    && t.checkExpect(coffee.sameDrink(tea), false)
	    && t.checkExpect(tea.sameDrink(coffee), false)
	    && t.checkExpect(baileys.sameDrink(coffee), false)
	    && t.checkExpect(coffee.sameDrink(baileys), false);
    }
}