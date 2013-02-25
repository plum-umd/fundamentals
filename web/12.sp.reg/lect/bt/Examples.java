import tester.*;

class Examples {
    IBT<Integer> mt = new Leaf<Integer>();
    IBT<Integer> n1 = new Node<Integer>(7, mt, mt);
    IBT<Integer> n2 = new Node<Integer>(8, mt, mt);
    IBT<Integer> n3 = new Node<Integer>(2, n1, n2);
    IBT<Integer> n4 = new Node<Integer>(1, mt, mt);
    IBT<Integer> n5 = new Node<Integer>(4, n3, n1);

    IBT<String> smt = new Leaf<String>();
    IBT<String> s1 = new Node<String>("D", smt, smt);
    IBT<String> s2 = new Node<String>("C", s1, smt);
    IBT<String> s3 = new Node<String>("B", smt, smt);
    IBT<String> s4 = new Node<String>("A", s3, s2);

    void testStringAppend(Tester t) {
	t.checkExpect(smt.accept(new StringAppend()), "");
	t.checkExpect(s4.accept(new StringAppend()), "BADC");
    }


    void testBiggest(Tester t) {
	t.checkExpect(n1.accept(new Biggest()), 7);
	t.checkExpect(n5.accept(new Biggest()), 8);
    }

    void testSize(Tester t) {
	t.checkExpect(n1.size(), 1);
        t.checkExpect(n3.size(), 3);
	t.checkExpect(n5.size(), 5);
    }

    void testSizeVisitor(Tester t) {
	t.checkExpect(n1.accept(new Size<Integer>()), 1);
        t.checkExpect(n3.accept(new Size<Integer>()), 3);
	t.checkExpect(n5.accept(new Size<Integer>()), 5);
    }
}
