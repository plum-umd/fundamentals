import tester.*;

class Examples {
    IBT<Integer> mt = new Leaf<Integer>();
    IBT<Integer> n1 = new Node<Integer>(7, mt, mt);
    IBT<Integer> n2 = new Node<Integer>(8, mt, mt);
    IBT<Integer> n3 = new Node<Integer>(2, n1, n2);
    IBT<Integer> n4 = new Node<Integer>(1, mt, mt);
    IBT<Integer> n5 = new Node<Integer>(4, n3, n1);

    Boolean testSize(Tester t) {
	return t.checkExpect(n1.size(), 1) 
	    && t.checkExpect(n2.size(), 3)
	    && t.checkExpect(n5.size(), 5);
    }

    Boolean testSizeVisitor(Tester t) {
	return t.checkExpect(n1.accept(new Size<Integer>()), 1)
	    && t.checkExpect(n2.accept(new Size<Integer>()), 3)
	    && t.checkExpect(n5.accept(new Size<Integer>()), 5);
    }
}
