import java.util.Map;
import java.util.HashMap;
import tester.*;

class Posn extends Object {
    Integer x;
    Integer y;
    Posn(Integer x, Integer y) {
	this.x = x;
	this.y = y;
    }


    public boolean equals(Object o) {
	if (o instanceof Posn) {
	    Posn that = (Posn)o;
	    return this.x == that.x
		&& this.y == that.y;
	} else {
	    return false;
	}
    }

    public int hashCode() {
	return this.x + this.y ;
    }
}

class Examples {
    Map<String, Integer> grades = new HashMap<String, Integer>();

    Map<Posn, String> board = new HashMap<Posn, String>();

    void testGet(Tester t) {
	grades.put("DVH", 62);
	grades.put("VKP", 97);
	grades.put("MF", 62);
	grades.put("Hoare", null);

	t.checkExpect(grades.get("DVH"), 62);
	t.checkExpect(grades.get("Fred"), null);
	t.checkExpect(grades.get("Hoare"), null);
	
	grades.put("DVH", 93);
	t.checkExpect(grades.get("DVH"), 93);

	t.checkExpect(grades.containsKey("Fred"), false);
	t.checkExpect(grades.containsKey("Hoare"), true);
    }

    void testGetPosn(Tester t) {
	Posn origin = new Posn(0,0);
	board.put(origin, "DVH");
	board.put(new Posn(3,4), "VKP");
	board.put(new Posn(5,5), "DVH");

	t.checkExpect(origin.equals(origin), true);

	t.checkExpect(board.get(origin), "DVH");

	t.checkExpect(new Posn(5,5).equals(new Posn(5,5)), true);
	t.checkExpect(board.get(new Posn(5,5)), "DVH");
	
    }

}