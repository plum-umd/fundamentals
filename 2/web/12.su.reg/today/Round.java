import tester.*;

class Examples {
    void testRound(Tester t) {
	Integer x = 3 / 4;
	Integer y = 1 / 4;
	Integer z = 5 / 4;
	t.checkExpect(x, 0);
	t.checkExpect(y, 0);
	t.checkExpect(z, 1);
    }
}