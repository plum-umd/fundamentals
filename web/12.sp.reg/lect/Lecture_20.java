/***********************************************
 *  CS2510 Spring 2011 
 *  Lecture #20
 *  Loops in all forms (Recursive, While, and For)
 ***********************************************/

import tester.*;
import java.util.ArrayList;


/** Represents a Predicate */
interface ISelect<T>{
    /** Selected? */
    boolean select(T t);
}

/** Class that implements different Algorithms */
class Algos{
    /** Count the number of Selected Elements from the given Traversal */
    <T> int countSuch(Traversal<T> tr, ISelect<T> comp){
        // Call ACC method with the BASE-VALUE
        return this.countAcc(tr, comp, 0);
    }
    
    /** Count using an Accumulator
     *    ACC: The number selected so far */
    <T> int countAcc(Traversal<T> tr, ISelect<T> comp, int acc){
        // BASE-CASE (TERMINATION-PREDICATE 2000
        if(tr.isEmpty()){
            return acc;
        }else{
            // THE RECURSIVE CALL
            return this.countAcc(
                    // ADVANCE
                    tr.getRest(), comp,
                    // UPDATE NEW ACC
                    this.updateAcc(tr.getFirst(), comp, acc));
        }
    }
    /** Update the Accumulator for the countAcc Method */
    <T> int updateAcc(T first, ISelect<T> comp, int acc){
        if(comp.select(first))
            return acc+1;
        else{
            return acc;
        }
    }
    
    /** Count the Selected elements using a While Loop */
    <T> int countWhile(Traversal<T> tr, ISelect<T> comp){
        // INIT
        int acc = 0;
        // CONTINUATION-CONDITION
        while(!tr.isEmpty()){
            // UPDATE
            acc = this.updateAcc(tr.getFirst(), comp, acc);
            // ADVANCE
            tr = tr.getRest();
        }
        // POST STUFF
        return acc;
    }
    
    /** Count the Selected elements using a For Loop */
    <T> int countFor(Traversal<T> tr, ISelect<T> comp){
        // INIT
        int acc = 0;
        for(;
            // CONTINUATION-CONDITION
            !tr.isEmpty();
            // ADVANCE
            tr = tr.getRest())
        {
           // UPDATE 
           acc = this.updateAcc(tr.getFirst(), comp, acc);
        }
        // POST STUFF
        return acc;
    }
}

/** Represents a Traversal of an ArrayList */
class ALTrav<T> implements Traversal<T>{
    ArrayList<T> alist;
    int index;
    
    ALTrav(ArrayList<T> alist){
        this(alist, 0);
    }
    
    private ALTrav(ArrayList<T> alist, int index){
        this.alist = alist;
        this.index = index;
    }
    
    /** Is this Traversal Empty? */
    public boolean isEmpty(){
        return this.index >= this.alist.size();
    }
    /** Get the first Element of this Traversal */
    public T getFirst(){
        return this.alist.get(this.index);
    }
    /** Get a Traversal representing the rest of this one */
    public Traversal<T> getRest(){
        return new ALTrav<T>(this.alist, this.index+1);
    }
    
}

/** Select Odd Numbers */
class Odd implements ISelect<Integer>{
    Odd(){}
    
    /** Is the given number Odd? */
    public boolean select(Integer i){
        return i % 2 == 1;
    }
}

/** Select Even Numbers */
class Even implements ISelect<Integer>{
    Even(){}
    
    /** Is the given number Even? */
    public boolean select(Integer i){
        return i % 2 == 0;
    }
}

/** Custom Exception Subclass (must be declared/caught) */
class Ex extends Exception{
    Ex(String msg){
        super(msg);
    }
}

/** Custom RuntimeException Subclass (thrown/caught at will) */
class REx extends RuntimeException{
    REx(String msg){
        super(msg);
    }
}

/** Exception Examples Class */
class ExceptionExamples{
    
    int mustCatch() throws Ex{
        throw new Ex("Have to Declare Me!");
    }
    int noNeedToCatch(){
        throw new REx("If you want...");
    }
    
    int clientCatch(){
        // Optional Catching and Recovery
        try{
            return this.noNeedToCatch();
        }catch(REx e){
            // Handle the specific REx exception...
            System.err.println("Got REx Exception: "+e);
        }catch(RuntimeException e){
            // Handle other RuntimeExceptions, if they 
            //   are not specifically "REx"
            System.err.println("Got REx Exception: "+e);
        }
        
        // Forced Catching
        try{
            return this.mustCatch();
        }catch(Ex e){
            // Handle the specific Ex exception...
            System.err.println("Got Ex Exception: "+e);
        }
        // Could also handle other Exceptions (even Runtime ones)

        // Since we handle all the exceptions, have to return something
        return 43;
    }

    // Instead of catching the Exception, we can declare that we may
    //   also throw it (if it's possible, then we must)
    int clientNoCatch() throws Ex {
        // When the Exception is thrown, we never return
        return this.mustCatch();
    }
}


/** Examples and Tests */
class LectureExamples{
    ArrayList<Integer> alist = new ArrayList<Integer>();
    ISelect<Integer> odd = new Odd();
    ISelect<Integer> even = new Even();
    Algos algos = new Algos();
    
    void init(){
        this.alist.clear();
        this.alist.add(1);
        this.alist.add(2);
        this.alist.add(3);
        this.alist.add(4);
        this.alist.add(5);
    }
    
    // Test our recursive, while, and for loop implementations of countSuch
    void testLoops(Tester t){
        this.init();
        
        t.checkExpect(this.algos.countSuch(new ALTrav<Integer>(this.alist), this.odd), 3);
        t.checkExpect(this.algos.countSuch(new ALTrav<Integer>(this.alist), this.even), 2);
        
        t.checkExpect(this.algos.countAcc(new ALTrav<Integer>(this.alist), this.odd, 2), 5);
        t.checkExpect(this.algos.countAcc(new ALTrav<Integer>(this.alist), this.even, 3), 5);
        
        t.checkExpect(this.algos.countWhile(new ALTrav<Integer>(this.alist), this.odd), 3);
        t.checkExpect(this.algos.countWhile(new ALTrav<Integer>(this.alist), this.even), 2);
        
        t.checkExpect(this.algos.countFor(new ALTrav<Integer>(this.alist), this.odd), 3);
        t.checkExpect(this.algos.countFor(new ALTrav<Integer>(this.alist), this.even), 2);
    }
}
