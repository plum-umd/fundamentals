import tester.*;

// Import the ArrayList class
import java.util.ArrayList;

// Represents a Dinosaur
class Dino{
    String eyecolor;
    Integer teeth;
    
    Dino(String eyecolor, Integer teeth){
        this.eyecolor = eyecolor;
        this.teeth = teeth;
    }
}

// Examples and Tests of mutation and ArrayList methods
class LectureExamples{
    // The initial List
    ArrayList<Dino> alist = new ArrayList<Dino>();
    
    // Some Dinos for testing
    Dino d1 = new Dino("blue", 2);
    Dino d2 = new Dino("yellow", 70);
    Dino d3 = new Dino("black", 6);
    Dino d4 = new Dino("chartreuse", 50);
    Dino d5 = new Dino("red", 8);
    Dino d6 = new Dino("magenta", 40);
    Dino d7 = new Dino("indigo", 33);
    
    // Reset our data... add the first three Dinos
    void reset(){
        this.alist.clear();
        this.alist.add(this.d1);
        this.alist.add(this.d2);
        this.alist.add(this.d3);
    }
    
    // Examples/Tests of all the ArrayList methods
    void testAll(Tester t){
        /** Useful methods for ArrayList<E>
         *     void clear()
         *     boolean isEmpty()
         *     int size()
         *     boolean add(E element)
         *     void add(int index, E element)
         *     E get(int index)
         *     E set(int index, E element)
         *     E remove(int index)
         */

        this.reset();
        t.checkExpect(this.alist.get(1).eyecolor, "yellow");
        t.checkExpect(this.alist.get(2).eyecolor, "black");

        // Throws an Exception when:  size() <= index < 0
        t.checkException(new IndexOutOfBoundsException("Index: 6, Size: 3"),
                this.alist, "add", 6, this.d4);
        
        this.alist.remove(2);
        t.checkExpect(this.alist.size(), 2);
        
        this.alist.add(2, this.d3);
        t.checkExpect(this.alist.size(), 3);
        t.checkExpect(this.alist.get(2).teeth, 6);
        
        this.alist.clear();
        t.checkExpect(this.alist.isEmpty(), true);
        t.checkExpect(this.alist.size(), 0);
        
        this.reset();
        this.alist.set(0, this.d4);
        t.checkExpect(this.alist.size(), 3);
        t.checkExpect(this.alist.get(0).eyecolor, "chartreuse");
        
        this.alist.remove(0);
        t.checkExpect(this.alist.get(0), this.d2);

        // Test our Swap method
        this.reset();
        this.swap(this.alist, 0, 2);
        t.checkExpect(this.alist.get(0).eyecolor, "black");
        t.checkExpect(this.alist.get(2).eyecolor, "blue");
    }
    
    // Reset and add all the data in order of
    //   the number of Teeth
    void reset2(){
        this.alist.clear();
        this.alist.add(this.d1);
        this.alist.add(this.d3);
        this.alist.add(this.d5);
        this.alist.add(this.d7);
        this.alist.add(this.d6);
        this.alist.add(this.d4);
        this.alist.add(this.d2);
    }
    
    // EFFECT: Swap the two elements of the given list
    <X> void swap(ArrayList<X> alist, int i, int j){
        alist.set(j, alist.set(i, alist.get(j)));
    }
    
    // Find the dino with the given num of teeth using binary search
    // We search in the given list between indexes low and high for
    //   the Dino with the given number of teeth
    Dino search(int teeth, ArrayList<Dino> dlst, int low, int high){
        /** Quick Template
         *     dlst.get(int)
         *     this.search(int, ArrayList<Dino>, int, int)
         */
        // The middle is a good guess
        int middle = (low+high)/2;
        
        // Ran out of items to search through
        if(low > high){
            throw new RuntimeException("$&#%*@ Happens");
        }
        
        // Found it!!
        if(teeth == dlst.get(middle).teeth){
            return dlst.get(middle);
        }else{
            if(teeth > dlst.get(middle).teeth){
                // Search to the right... more teeth
                return this.search(teeth, dlst, middle+1, high);
            }else{
                // Search to the left... less teeth
                return this.search(teeth, dlst, low, middle-1);
            }
        }
    }
    // Test our binary search
    void testTeeth(Tester t){
        this.reset2();
        t.checkExpect(this.search(2, this.alist, 0, this.alist.size()), this.d1);
        t.checkExpect(this.search(70, this.alist, 0, this.alist.size()), this.d2);

        // Bad number of teeth...
        t.checkException(new RuntimeException("$&#%*@ Happens"),
                         this, "search", 14, this.alist, 0, this.alist.size());
    }   
}
