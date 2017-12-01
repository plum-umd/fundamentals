import java.util.Comparator;

interface Max<X> extends Comparator<X> {
    X pick(X x1, X x2);
}

public class LongerString implements Comparator<String>, Max<String> {
    public Integer compare(String s1, String s2) {
	if (s1.length() < s2.length()) {
	    return -1;
	} else if (s1.length() > s2.length()) {
	    return 1;
	} else {
	    return 0;
	}
    }

    public String pick(String s1, String s2) {
	if (s1.length() < s2.length()) {
	    return s2;
	} else {
	    return s1;
	}
    }
}