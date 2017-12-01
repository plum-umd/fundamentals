// CS 2510 Spring 2012
// Lab 4

import tester.*;

// to represent a list of Strings
interface ILoS{
  // combine all Strings in this list into one
  String combine();
}

// to represent an empty list of Strings
class MtLoS implements ILoS{
  MtLoS(){}
  
  // combine all Strings in this list into one
  String combine(){
    return "";
  }  
}

// to represent a nonempty list of Strings
class ConsLoS implements ILoS{
  String first;
  ILoS rest;
  
  ConsLoS(String first, ILoS rest){
    this.first = first;
    this.rest = rest;  
  }

 /*
  TEMPLATE
  FIELDS:
  ... this.first ...         -- String
  ... this.rest ...          -- ILoS
  
  METHODS
  ... this.combine() ...      -- String
  
  METHODS FOR FIELDS
  ... this.first.concat(String) ...       -- String
  ... this.first.compareTo(String) ...    -- int
  ... this.rest.combine() ...             -- String
  
  */
  
  // combine all Strings in this list into one
  String combine(){
    return this.first.concat(this.rest.combine());
  }  

}

// to represent examples for lists of strings
class ExamplesStrings{
  ExamplesStrings(){}
  
  ILoS mary = new ConsLoS("Mary ",
               new ConsLoS("had ",
                new ConsLoS("a ",
                 new ConsLoS("little ",
                  new ConsLoS("lamb.", new MtLoS())))));
                          
  boolean testCombine(Tester t){
    return 
    t.checkExpect(this.mary.combine(), "Mary had a little lamb.");
  }
}