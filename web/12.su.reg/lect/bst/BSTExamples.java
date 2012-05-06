import tester.*;

class BSTExamples {
    IBT<String> mt = new Leaf<String>();
    IBT<String> n1 = new Node<String>("Alice", mt, mt);
    IBT<String> n2 = new Node<String>("Carol", mt, mt);
    IBT<String> n3 = new Node<String>("Bobby", n1, n2);
    IBT<String> n4 = new Node<String>("Bobby", mt, n2);
    
    IBST<String> smt = new BST<String>(new Alpha(), mt);
    IBST<String> s1 = new BST<String>(new Alpha(), n1);
    IBST<String> s2 = new BST<String>(new Alpha(), n3);
    IBST<String> s3 = new BST<String>(new Alpha(), n4);

    void testSize(Tester t) {
	t.checkExpect(s1.size(), 1);
	t.checkExpect(s2.size(), 3);
    }

    void testMax(Tester t) {
	t.checkExpect(s1.max(), "Alice");
	t.checkExpect(s1.min(), "Alice");
	t.checkExpect(s2.max(), "Carol");
	t.checkExpect(s2.min(), "Alice");
    }

    void testInsert(Tester t) {
	t.checkExpect(smt.insert("Alice"), s1);
	t.checkExpect(s3.insert("Alice"), s2);
    }
}
	    