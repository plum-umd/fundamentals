import tester.*;

interface Function<A,B> {
    B call(A x);
}

class Square implements Function<Double,Double> {
    public Double call(Double x) { return x * x; }
}

class Derivative 
    implements Function<Function<Double,Double>,
	       Function<Double,Double>> {
    public Function<Double,Double> call(final Function<Double,Double> f) {
	final Double d = 0.0000001;
	return new Function<Double,Double>() {
	    public Double call (Double x) {
		return (f.call(x) - f.call(x-d))/d;
	    } 
	} ;
    }
}

class Examples {

    void test(Tester t) {
	Square s = new Square();
	Derivative d = new Derivative();
	t.checkExpect(d.call(s).call(5.0), 10.0);
	t.checkExpect(d.call(new Function<Double,Double>() {
		    public Double call(Double x) { 
			return Math.sin(x);
		    }
		}).call(Math.PI), -1.0);
    }
}