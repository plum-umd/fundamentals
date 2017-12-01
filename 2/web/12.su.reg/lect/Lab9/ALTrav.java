import tester.*;
import java.util.*;

/** Represents an functional Traversal of an ArrayList */
class ALTrav<T> implements Traversal<T>{

    ArrayList<T> alist;
    int index;

    /** Public constructor for the <code>ArrayList</code> we wish to traverse */
    public ALTrav(ArrayList<T> alist){
        this.alist = alist;
        this.index = 0;
    }

    /** Private constructor to produce the <code>Traversal</code> for the 
     *    rest of the <code>ArrayList</code> we are currently traversing */
    private ALTrav(ArrayList<T> alist, int index){
        this.alist = alist;
        this.index = index;
    }


    /** Is this traversal complete? */
    public boolean isEmpty(){ 
        return this.index >= this.alist.size();
    }  

    /** Produce the next element in this traversal */
    public T getFirst(){
        if(this.isEmpty()){
            throw new IllegalUseOfTraversalException("No first element");
        }else{
            return this.alist.get(this.index);
        }
    }

    /** Produce a <code>Traversal</code> for the rest the lists elements */
    public Traversal<T> getRest(){
        if(this.isEmpty()){
            throw new IllegalUseOfTraversalException("No more elements");
        }else{
            return new ALTrav<T>(this.alist, this.index + 1);
        }
    }
} 
