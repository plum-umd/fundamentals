/***********************************************
 *  CS2510 Spring 2011 
 *  Lecture #22
 *  Iterable Cons Lists, and St. Pat. Day
 ***********************************************/

import java.util.*;
import tester.*;
import image.*;
import world.*;

/** Represents a generic List of Elements */
interface ILo<L> extends Iterable<L>{
    /** Is it empty? */
    boolean isEmpty();
    /** Get the first of this list */
    L getFirst();
    /** Get the rest of this list */
    ILo<L> getRest();
}

/** Represents a generic nonempty List of Elements */
class Cons<L> implements ILo<L>{
    L first;
    ILo<L> rest;
    
    Cons(L first, ILo<L> rest) {
        this.first = first;
        this.rest = rest;
    }
    
    /** Is it empty? */
    public boolean isEmpty(){ return false; }
    /** Get the first of this list */
    public L getFirst(){ return this.first; }
    /** Get the rest of this list */
    public ILo<L> getRest(){ return this.rest; }
    
    /** Get the iterator for this list */
    public Iterator<L> iterator(){
        return new ListIter<L>(this);
    }
}

/** Represents a generic empty List of Elements */
class Mt<L> implements ILo<L>{
    Mt(){}
    
    /** Is it empty? */
    public boolean isEmpty(){ return true; }
    /** Get the first of this list */
    public L getFirst(){ throw new RuntimeException("No First"); }
    /** Get the rest of this list */
    public ILo<L> getRest(){ throw new RuntimeException("No Rest"); }
    
    /** Get the iterator for this list */
    public Iterator<L> iterator(){
        return new ListIter<L>(this);
    }
}

/** Iterator for our generic Lists */
class ListIter<L> implements Iterator<L>{
    // The list we are iterating
    ILo<L> llist;
    
    ListIter(ILo<L> llist){
        this.llist = llist;
    }
    
    /** Is there a next element? */
    public boolean hasNext(){
        return !this.llist.isEmpty();
    }
    /** Returns the current element
     *    EFFECT: Advances to the next Element */
    public L next(){
        if(!this.hasNext()){
            throw new RuntimeException("Nothing there");
        }
        L fst = this.llist.getFirst();
        this.llist = this.llist.getRest();
        return fst;
    }
    /** Not implemented. Though this method is part of the Iterator
     *    interface, it is rarely implemented */
    public void remove(){
        throw new RuntimeException("Can't Remove");
    }
}

class LectureExamples{
    ILo<Integer> alist;
    
    /** Test our iterator/iterable List */
    void testIt(Tester t){
        this.alist = 
            new Cons<Integer>(0, 
                    new Cons<Integer>(2, 
                            new Cons<Integer>(8, 
                                    new Cons<Integer>(4,
                                            new Mt<Integer>()))));
        
        int i = 0;
        for(Integer num : this.alist){
            System.out.println(" Element["+i+"]: "+num);
            i++;
        }
    }
    
    /** Test our shamrock drawing */
    void testThat(Tester t){
        boolean fred = World.display(this.shammy(50, 3));
    }
    
    /** Create a clover-like Scene */
    Scene shammy(int size, int numLeafs){
        Scene acc = new EmptyScene(400, 400);
        
        // Radians per movement (fitting numLeafs
        //   into a full circle) */
        double rad = 2*Math.PI / numLeafs;
        
        // Draw a circle for each of the leafs
        for(int i = 0; i < numLeafs; i++){
            // Current angle/radians
            double curr = i*rad;
            // X/Y, scaled by the size
            double x = Math.cos(curr)*size;
            double y = Math.sin(curr)*size;
            
            // Put the image where we want it
            acc = acc.placeImage(new Circle(size, "solid", "forestgreen"),
                                 200+x, 200+y);
        }
        
        // Return the Scene
        return acc;
    }
    
}






