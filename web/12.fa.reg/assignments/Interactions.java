/**
 * Interactions driver for the Eliza game.
 * 
 * @since 19 March 2012 
 */

import java.io.*;

public class Interactions{
  
  /**
   * Run the program - starting with the <code>eliza</code>
   * method.
   * 
   * @param args unused
   */
  public static void main(String[] args) {   
    
    Interactions i = new Interactions();
    i.eliza();
  }
  
  /**
   * Run the Eliza game
   */ 
  public void eliza(){
  
    BufferedReader input =
      new BufferedReader(new InputStreamReader(System.in));
    
    // define data you need to play the game ... REPLACE THIS WITH YOUR CODE !!!
    // ...
    
    System.out.println("Ask me a question. \n:>");
    try{
      String s = input.readLine();
      while (!(s == null) && s.length() > 0){

        
        // mock code: echo every line
        System.out.println(s);   // REPLACE THIS WITH YOUR CODE !!!
                                 // that finds out the reply to the question s
                                 // and prints the reply
        
        System.out.println(":>");
        s = input.readLine();
        if (s == null || s.length() == 0){
          System.out.println("Goodbye");
        }
      }
    }
    catch(IOException e){
      System.out.println("Goodbye");
    }  
  }
}

