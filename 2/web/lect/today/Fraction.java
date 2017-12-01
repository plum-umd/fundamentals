import tester.*;

class Frac {
    Integer num;
    Integer den;
    Frac(Integer num, Integer den) {
	Integer d = new GCD().gcd(num, den);
	if (den.equals(0)) {
	    throw new RuntimeException("Denominator can't be zero");
	} else {
	    this.num = num / d;
	    this.den = den / d;
	}
    }

    Frac(Integer num) {
	this(num, 1, true);
    }

    private Frac(Integer num, Integer den, Boolean bs) {
	this.num = num;
	this.den = den;
    }

    Frac someMethod() {
	return new Frac(3, 4, true);
    }

    Boolean sameAs(Frac that) {
	return this.num.equals(that.num)
	    && this.den.equals(that.den);
    }
}

class GCD {
    public Integer gcd(Integer a, Integer b) {
	if (b.equals(0))
	    return a;
	else
	    return gcd(b, a % b);
    }
}

class Examples {
    Frac f = new Frac(3, 4);
    Frac g = new Frac(6, 8);
    Frac h = new Frac(10, 5);
    Frac i = new Frac(2);
    Frac j = new Frac(5, 0);
    void testSameAs(Tester t) {
	t.checkExpect(f.sameAs(f), true);
	t.checkExpect(f.sameAs(g), true);
	t.checkExpect(h.sameAs(i), true);
    }
}