import java.util.*;

// Represents lexicographic order on strings
public class Alpha implements Comparator<String> {
    public int compare(String s1, String s2) {
	return s1.compareTo(s2);
    }
}