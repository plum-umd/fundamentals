import tester.*;

// An example of how to test for exceptional behavior

// Represents a function A -> B
interface Function<A,B> {
    // Apply this function to given argument.
    B apply(A a);
}

// Represents inversion operation
class Divider implements Function<Double,Double> {
    Divider() {}

    // Produce 1/x.
    public Double apply(Double x) {
	if (x.equals(0.0)) {
	    throw new RuntimeException("Divide by zero.");
	} else {
	    return 1/x;
	}
    }
}

class Examples {
    void testInv(Tester t) {
	t.checkException(// The expected exception
			 new RuntimeException("Divide by zero."),
			 // The object that throws
			 new Divider(),
			 // The method (as a string!) that throws
			 "apply",
			 // The arguments to the method
			 0.0);
    }
}