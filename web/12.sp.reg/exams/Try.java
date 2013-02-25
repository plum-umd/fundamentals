import java.util.*;

interface DTree{
  /** The original list of characters used in encoding and decoding */
  ArrayList<Character> alphabet = 
        new ArrayList<Character>(Arrays.asList(
            'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 
            'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 
            't', 'u', 'v', 'w', 'x', 'y', 'z', ' '));
}

class Try implements DTree{
    public static void main(String[] args) {
	for (String s : args)
	    System.out.println(encode(s, ""));
    }

    static String encode(String source, String acc){
	if (source.length() > 0)
	    return encode(source.substring(1),
			  acc + alphabet.get((alphabet.indexOf(source.charAt(0)) + acc.length()) % 27));
	else
	    return acc;
    }

    static String decode(String source, String acc){
	if (source.length() > 0)
	    return decode(source.substring(1),
			  acc + alphabet.get((alphabet.indexOf(source.charAt(0)) - acc.length()+ 27) % 27));
	else
	    return acc;
    }
}
