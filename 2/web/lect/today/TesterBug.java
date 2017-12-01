import tester.*;
import java.util.ArrayList;

class Examples {

    void testGood(Tester t) {
	ArrayList<Integer> is = new ArrayList<Integer>();
	ArrayList<Integer> js = new ArrayList<Integer>();
	t.checkExpect(is, js);
    }

    void testBad(Tester t) {
	ArrayList<Integer> is = new ArrayList<Integer>() {{}};
	ArrayList<Integer> js = new ArrayList<Integer>() {{}};
	t.checkExpect(is, js);
    }
}
