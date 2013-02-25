import java.util.Comparator;

class LongerString implements Comparator<String> {
    Integer compare(String s1, String s2) {
	...
    }
}

class Biggest<X> implements IVisitorBT<X,X> {

    Comparator<X> comp;
    Biggest(Comparator<X> comp) {
	this.comp = comp;
    }

    public Integer visitLeaf() {
	throw new RuntimeException("No biggest number.");
    }

    public Integer visitNode(Integer val, 
		      IBT<Integer> left, 
		      IBT<Integer> right) {
	return Math.max(left.accept(new BiggestSoFar(val)),
			right.accept(new BiggestSoFar(val)));
    }
}

class BiggestSoFar implements IVisitorBT<Integer,Integer> {
    Integer seen;
    BiggestSoFar(Integer seen) {
	this.seen = seen;
    }

    public Integer visitLeaf() { return this.seen; }
    public Integer visitNode(Integer val, 
		      IBT<Integer> left, 
		      IBT<Integer> right) {
	return Math.max(val, Math.max(left.accept(this),
				      right.accept(this)));
    }
}
			      