import tester.*;

// Represents a rational number
class Rational {
    Integer numerator;
    Integer denominator;
    Rational(Integer numerator, Integer denominator) {
	this.numerator = numerator / this.gcd(numerator, denominator);
	this.denominator =  denominator / this.gcd(numerator, denominator);
    }

    Rational simplify() {
	return this;
    }

    Integer gcd(Integer a, Integer b) {
	if (b == 0) {
	    return a;
	} else {
	    return this.gcd(b, a % b);
	}
    }

    // Is this rational the same as that rational? (extensionally the same?)
    Boolean same(Rational that) {
	return this.numerator == that.numerator
	    && this.denominator == that.denominator;
    }

    // Is this intensionally the same as that?
    Boolean eq(Rational that) {
	Integer thisNum = this.numerator;
	Integer thatNum = that.numerator;

	this.numerator = -1;
	that.numerator = -2;

	Boolean res = this.numerator == -2;
	this.numerator = thisNum;
	that.numerator = thatNum;
	return res;
    }

    public boolean equals(Rational that) {
	return this.same(that);
    }

    public int hashCode() {
	return this.numerator + this.denominator;
    }
}

class Examples {
    void testSimplify(Tester t) {
	t.checkExpect(new Rational(3,4).simplify(),
		      new Rational(3,4));
	t.checkExpect(new Rational(6,8).simplify(),
		      new Rational(3,4));
	t.checkExpect(new Rational(6,8).same(new Rational(3,4)), true);
	t.checkExpect(new Rational(6,8), new Rational(3, 4));
	t.checkExpect(new Rational(3,4).equals(new Rational(3,4)), true);
	t.checkExpect(new Rational(3,4).equals("(3,4)"), false);
	t.checkExpect(new Rational(3,4).equals(new Rational(4,3)), false);
	t.checkExpect(new Rational(3,4).hashCode(),
		      new Rational(3,4).hashCode());
    }
}