
interface Fun<X,Y> {
    Y apply(X x);
}

class EmptyConsSizeOne implements Fun<Integer, Boolean> {
    Boolean apply(Integer i) {
	return new QEmpty<Integer>().cons(i).size() == 1;
    }
}


void testQList(Tester t) {

    Fun<Integer,Boolean> p = new EmptyConsSizeOne();

    QList<String> mt = new QEmpty<String>();
    t.checkExpect(mt.size(), 0);

    Random r = new Random();
    for (Integer j : new HalfOpen(0, 1000)) {
	t.checkExpect(p.apply(r.nextInt()), true);
    }

    t.checkExpect(p.apply(45), true);
    t.checkExpect(p.apply(36), true);


    // t.checkExpect(mt.cons("Hi").size(), 1);
    // t.checkExpect(mt.cons("Bye").size(), 1);
    t.checkExpect(mt.cons("Hi").cons("there").size(), 2);
    //...
}