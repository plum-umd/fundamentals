// Non-empty list of X
interface ConsListof<X> {
    // Finds the (first) element of the list that minimizes the output
    // of the function.
    X argmin(IFun<X,Double> f);
}

// List of X
interface Listof<X> {
    // Finds the (first) element of the list or minX, that minimizes
    // the output of the function.
    X argminAcc(IFun<X,Double> f, X minX);
}

// [X -> Y]
interface IFun<X,Y> {
    Y apply(X x);
}

class Empty<X> implements Listof<X> {
    Empty() {}

    public X argminAcc(IFun<X,Double> f, X minX) {
	return minX;
    }
}

class Cons<X> implements Listof<X>, ConsListof<X> {
    X first;
    Listof<X> rest;

    Cons(X first, Listof<X> rest) {
	this.first = first;
	this.rest = rest;
    }
    
    // Finds the (first) element of the list that minimizes the output
    // of the function.
    public X argmin(IFun<X,Double> f) {
	return this.rest.argminAcc(f, this.first);
    }

    public X argminAcc(IFun<X,Double> f, X minX) {
	Double fMinX = f.apply(minX);
	Double fThisX = f.apply(this.first);
	if (fThisX < fMinX) {
	    return this.rest.argminAcc(f, this.first);
	} else {
	    return this.rest.argminAcc(f, minX);
	}
    }		  
}

class Posn {
    Double x;
    Double y;

    Posn(Double x, Double y) {
	this.x = x;
	this.y = y;
    }

    // Euclidean distance between this and that posn.
    public Double dist(Posn that) {
	return Math.sqrt(Math.pow(this.x - that.x, 2) +
			 Math.pow(this.y - that.y, 2));
    }

    // Move this posn by given vector.
    public Posn moveBy(Posn v) {
	return new Posn(this.x + v.x, this.y + v.y);
    }

    // Choose best direction that moves this toward that posn.
    public Posn chooseDir(Posn that) {
	return Dir.directions.argmin(new DistMoveBy(this, that));
    }
}

// Represents: (λ (d) (dist (move-by origin d) dest))
class DistMoveBy implements IFun<Posn,Double> {
    Posn origin;
    Posn dest;
    DistMoveBy(Posn origin, Posn dest) {
	this.origin = origin;
	this.dest = dest;
    }

    // Move origin in given dir and compute dist to dest.
    public Double apply(Posn dir) {
	return this.origin.moveBy(dir).dist(this.dest);
    }
}

class Dir {
    static Posn E  = new Posn(1.0, 0.0);
    static Posn NE = new Posn (1 / Math.sqrt(2), 1 / Math.sqrt(2));
    static Posn N  = new Posn(0.0, 1.0); 
    static Posn NW = new Posn(-1 / Math.sqrt(2), 1 / Math.sqrt(2));
    static Posn W  = new Posn(-1.0, 0.0);
    static Posn SW = new Posn(-1 / Math.sqrt(2), -1 / Math.sqrt(2));
    static Posn S  = new Posn(0.0, -1.0);
    static Posn SE = new Posn(1 / Math.sqrt(2), -1 / Math.sqrt(2));
    static ConsListof<Posn> directions = 
	new Cons<Posn>(E,
		 new Cons<Posn>(NE,
			  new Cons<Posn>(N, 
				   new Cons<Posn>(NW,
					    new Cons<Posn>(W,
						     new Cons<Posn>(SW,
							      new Cons<Posn>(S,
								       new Cons<Posn>(SE,
										new Empty<Posn>()))))))));
}
