import java.util.*;

/**
 * A class that defines a new permutation code, as well as methods for encoding
 * and decoding of the messages that use this code.
 */
public class PermutationCode{
  /** The original list of characters to be encoded */
  ArrayList<Character> alphabet = 
        new ArrayList<Character>(Arrays.asList(
            'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 
            'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 
            't', 'u', 'v', 'w', 'x', 'y', 'z'));
  
  ArrayList<Character> code = new ArrayList<Character>(26);
  
  /** A random number generator */
  Random rand = new Random();
  
  /**
   * Create a new instance of the encoder/decoder with a new permutation code 
   */
  PermutationCode(){
    this.code = this.initEncoder();
  }
  
  /**
   * Create a new instance of the encoder/decoder with the given code 
   */
  PermutationCode(ArrayList<Character> code){
    this.code = code;
  }
  
  /** Initialize the encoding permutation of the characters */
  ArrayList<Character> initEncoder(){
    return this.alphabet;
  }
  
  /**
   * produce an encoded <code>String</code> from the given <code>String</code>
   * @param source the <code>String</code> to encode
   * @return the secretly encoded <String>
   */
  String encode(String source){
    return "";
  }
  
  /**
   * produce an decoded <code>String</code> from the given <code>String</code>
   * @param source the <code>String</code> to decode
   * @return the revealed <String>
   */
  String decode(String code){
    return "";
  }
}