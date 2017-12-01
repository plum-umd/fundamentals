/***********************************************
 *  CS2510 Spring 2011 
 *  Lecture #18
 *  ArrayList "Wrappers": Traversals, Stacks, and Queues
 ***********************************************/

import tester.*;
import java.util.ArrayList;


//** Review of function-classes/objects

// A Function-Interface
interface ISelect<T>{
    // Should we select the given T?
    public boolean huh(T that);
}
// A Function-Class
class Odder implements ISelect<Integer>{
    // Select odd Integers
    public boolean huh(Integer i){
        return (i % 2) == 1;
    } 
}
// A Function-Class
class Halfer implements ISelect<Double>{
    // Select doubles that end in ".5"
    public boolean huh(Double d){
        return (d % 1) == 0.5;
    } 
}

//** Note: a similar interface is provided by the Tester Library

// Represents the ordered Traversal of some data 
interface Traversal<W>{
    // Is it empty
    boolean emptyHuh();
    // Get the first of this Traversal (a W)
    W getFirst();
    // Get the rest of this Traversal (another Traversal)
    Traversal<W> getRest();
}

// A functional ("immutable") Traversal Wrapper for ArrayLists
class ALTrav<R> implements Traversal<R>{
    ArrayList<R> alist;
    int index;
    
    // Private Constructor for all fields 
    private ALTrav(ArrayList<R> alist, int index){
        this.alist = alist;
        this.index = index;
    }
    // Public constructor starts at the beginning 
    public ALTrav(ArrayList<R> alist){
        this(alist, 0);
    }
    
    //*** Might also think about error checking ***
    
    // Empty when we are off the end
    public boolean emptyHuh(){
        return this.index == this.alist.size();
    }
    // Return the first (current) element
    public R getFirst(){
        return this.alist.get(index);
    }
    // Return the rest of the Traversal
    public Traversal<R> getRest(){
        return new ALTrav<R>(this.alist, this.index+1);
    }
}

// Represents a Stack of elements by wrapping an ArrayList
class Stack<S>{
    ArrayList<S> alist = new ArrayList<S>();

    // Starts Empty
    Stack(){}

    // Add a new element to this Stack (at the top)
    void push(S s){
        this.alist.add(s);
    }
    // Return and remove the last element added
    S pop(){
        if(this.alist.isEmpty()){
            throw new RuntimeException("Mimi says bad!");
        }else
            return this.alist.remove(this.alist.size()-1);
    }
    // Take a peek at the top of this Stack
    S peek(){
        if(this.alist.isEmpty()){
            throw new RuntimeException("No pancakes for you!");
        }else
            return this.alist.get(this.alist.size()-1);
    }
}


class Queue<Q>{
    ArrayList<Q> alist = new ArrayList<Q>();
    
    Queue(){}
    
    // Add a new element to the "back" of this Queue
    void enqueue(Q s){
        this.alist.add(s);
    }
    // Return and remove the first element added
    Q dequeue(){
        if(this.alist.isEmpty()){
            throw new RuntimeException("Mimi says bad (Queue)!");
        }else
            return this.alist.remove(0);
    }
    // Take a peek at the front of this Queue
    Q peek(){
        if(this.alist.isEmpty()){
            throw new RuntimeException("No people for you!");
        }else
            return this.alist.get(0);
    }
}

// Represents a Pancake
class Pancake{
    int size;
    boolean whippedCreamHuh = true;
    
    Pancake(int size){
        this.size = size;
    }
}

// Represents a Person
class People{
    String eyecolor;
    boolean teethHuh;
    int height;
    
    People(String eyecolor, boolean teethHuh, int height) {
        this.eyecolor = eyecolor;
        this.teethHuh = teethHuh;
        this.height = height;
    }
}


class LectureExamples{
    // A Function-Object
    ISelect<Integer> odder = new Odder();
    
    // Lists, Stacks, and Queues
    ArrayList<Integer> ilist;
    Stack<Pancake> pstack;
    Queue<People> pq;

    // Reset all our Data...
    void reset(){
        // Reset the Array List
        ilist = new ArrayList<Integer>();
        ilist.add(7);
        ilist.add(5);
        ilist.add(12);
        ilist.add(4);
        ilist.add(37);
        
        // Reset our Stack of Pancakes
        pstack = new Stack<Pancake>();
        pstack.push(new Pancake(5));
        pstack.push(new Pancake(6));
        pstack.push(new Pancake(7));
        pstack.push(new Pancake(8));
        
        // Reset our Queue of People
        pq = new Queue<People>();
        pq.enqueue(new People("blue", true, 3));
        pq.enqueue(new People("red", false, 6));
        pq.enqueue(new People("chartreuse", true, 7));
        pq.enqueue(new People("parple", false, 82));
    }
    
    // Test all the Traversal, Stack, and Queue methods 
    void testAll(Tester t){
        reset();
        
        t.checkExpect(ilist.get(3), 4);
        
        ALTrav<Integer> bobby_walker = new ALTrav<Integer>(this.ilist);
        
        t.checkExpect(bobby_walker.emptyHuh(), false);
        t.checkExpect(bobby_walker.getFirst(), 7);
        t.checkExpect(bobby_walker.getRest().getFirst(), 5);
        t.checkExpect(bobby_walker.getRest().getRest().getRest()
                .getRest().getRest().emptyHuh(), true);
        
        t.checkExpect(pstack.peek(), new Pancake(8));
        t.checkExpect(pstack.pop(), new Pancake(8));
        t.checkExpect(pstack.pop(), new Pancake(7));
        t.checkExpect(pstack.peek(), new Pancake(6));
        t.checkExpect(pstack.peek().whippedCreamHuh, true);
        
        t.checkExpect(pq.peek().eyecolor, "blue");
        t.checkExpect(pq.dequeue().eyecolor, "blue");
        t.checkExpect(pq.dequeue().height, 6);
        t.checkExpect(pq.peek().eyecolor, "chartreuse");
        t.checkExpect(pq.peek().height, 7);
    }
}
