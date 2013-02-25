








// Represents a computation over a list of X producing a Result
interface ListVisitor<X,Result> {
    Result visitEmpty();
    Result visitCons(X first, List<X> rest);
}

interface List<X> {
    <R> R accept(ListVisitor<X,R> v);
}

class Empty<X> implements List<X> {
    Empty() {}
    public <R> R accept(ListVisitor<X,R> v) {
	return v.visitEmpty();
    }
}

class Cons<X> implements List<X> {
    X first;
    List<X> rest;
    Cons(X first, List<X> rest) {
	this.first = first;
	this.rest = rest;
    }
    public <R> R accept(ListVisitor<X,R> v) {
	return v.visitCons(this.first, this.rest);
    }
}

class Contains implements ListVisitor<String,Boolean> {
    String element;
    Contains(String element) {
	this.element = element;
    }
    public Boolean visitEmpty() {
	return false;
    }

    public Boolean visitCons(String first, List<String> rest) {
	return first.equals(this.element)
	    || rest.accept(this);
    }
}

/*
 ;; [List Integer] -> Integer
 (define (max ls)
    (cond [(empty? ls) (error "boo")]
          [(cons? ls) (max-acc (rest ls) (first ls))]))

 ;; [List Integer] Integer -> Integer
 (define (max-acc ls a)
   (cond [(empty? ls) a]
         [(cons? ls)
          (cond [(> a (first ls)) (max-acc (rest ls) a)]
                [else             (max-acc (rest ls) (first ls))])]))
 */

class Max implements ListVisitor<Integer,Integer> {
    public Integer visitEmpty() {
	throw new RuntimeException("boo");
    }
    public Integer visitCons(Integer first, List<Integer> rest) {
	return rest.accept(new MaxAcc(first));
    }
}

class MaxAcc implements ListVisitor<Integer,Integer> {
    Integer a;
    MaxAcc(Integer a) {
	this.a = a;
    }
    public Integer visitEmpty() {
	return this.a;
    }
    public Integer visitCons(Integer first, List<Integer> rest) {
	if (this.a > first) {
	    return rest.accept(this);
	} else {
	    return rest.accept(new MaxAcc(first));
	}
    }
}

class Examples {
    Examples() {}

    List<String> mt = new Empty<String>();
    List<String> l1 = new Cons<String>("hi", new Cons<String>("there", mt));

    void testContains(Tester t) {
	t.checkExpect(l1.accept(new Contains("there")), true);
	t.checkExpect(l1.accept(new Contains("bye")), false);
    }

    void testMax(Tester t) {
	List<Integer> li1 =
	    new Cons<Integer>(3, new Cons<Integer>(42, new Empty<Integer>()));
	t.checkExpect(li1.accept(new Max()), 42);
    }

    void testSame(Tester t) {
	t.checkExpect(new Sq(5).same(new Sq(5)), true);
	t.checkExpect(new Circ(5).same(new Circ(5)), true);
	t.checkExpect(new Circ(5).same(new Sq(5)), false);
    }
}
