/***********************************************
 *  CS2510 Spring 2011 
 *  Lecture #6
 *  Lists and Insertion Sorting
 ***********************************************/
import tester.*;

// Represents a List Of Strings
interface ILoS{
    // To Sort this list of strings by length()
    ILoS sort();
    // To insert a string into this *sorted* LoS
    ILoS insert(String s);
}

// Represents an empty List Of Strings
class MtLoS implements ILoS{
    MtLoS(){}
    
    /* Template
     * 
     *   Methods
     *     ... this.sort()         -- ILoS (Sorted)
     *     ... this.insert(String) -- ILoS (Sorted)
     *  
     */
    // To Sort this empty list of strings by length()
    ILoS sort(){
        return this;
    }
    // To insert a string into this empty *sorted* LoS
    ILoS insert(String s){
        return new ConsLoS(s, this);
    }
}

// Represents a nonempty List Of Strings
class ConsLoS implements ILoS{
    String first;
    ILoS rest;
    
    ConsLoS(String first, ILoS rest){
        this.first = first;
        this.rest = rest;
    }
    
    /* Template
     *   Fields
     *     ... this.first  -- String
     *     ... this.rest   -- ILoS
     * 
     *   Methods
     *     ... this.sort()         -- ILoS
     *     // "this" must be sorted for insert to work
     *     ... this.insert(String) -- ILoS (Sorted)
     *     
     *   Methods for Fields
     *     ... this.first.length()      -- int
     *     ... this.rest.sort()         -- ILoS
     *     // "rest" must be sorted for insert to work
     *     ... this.rest.insert(String) -- ILoS (Sorted)
     */
    
    // To Sort this nonempty list of strings by length()
    ILoS sort(){
        return (this.rest.sort()).insert(this.first);
    }
    // To insert a string into this *sorted* nonempty LoS
    ILoS insert(String s){
        if(s.length() < this.first.length()){
            return new ConsLoS(s, this);
        }else{
            return new ConsLoS(this.first, this.rest.insert(s));
        }
    }
}


// List, Sort and Insert Examples and Tests
class LoSExamples{
    ILoS los1 = new MtLoS();
    ILoS los2 = new ConsLoS("Liam", this.los1);    
    ILoS los3 = new ConsLoS("William", new ConsLoS("Billy", this.los2));
    ILoS los3s = new ConsLoS("Liam",
                       new ConsLoS("Billy",
                             new ConsLoS("William",
                                         this.los1)));
    ILoS los4 = new ConsLoS("Will",
            new ConsLoS("Kevin", this.los2));
    

    // Test our Sort method...
    boolean testSort(Tester t){
        return (t.checkExpect(this.los1.sort(), this.los1) &&
                t.checkExpect(this.los2.sort(), this.los2) &&
                t.checkExpect(this.los3.sort(), this.los3s));
    }
    // Test our Insert method...
    boolean testInsert(Tester t){
        return (t.checkExpect(this.los1.insert("Mike"),
                              new ConsLoS("Mike", this.los1)) &&
                t.checkExpect(this.los2.insert("Li"),
                              new ConsLoS("Li", this.los2)) &&
                t.checkExpect(this.los2.insert("Bruno"),
                              new ConsLoS("Liam",
                                          new ConsLoS("Bruno",
                                                      this.los1))));
    }

}