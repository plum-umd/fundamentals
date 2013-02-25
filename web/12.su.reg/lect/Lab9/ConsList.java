import tester.Traversal;

/** Represents a nonempty list of T */
public class ConsList<T> implements IList<T>{

    T first;
    IList<T> rest;
    
    public ConsList(T first, IList<T> rest){
        this.first=first;
        this.rest=rest;
    }
    
    /** Is this list empty? */
    public boolean isEmpty(){ return false; }  
    
    /** Produce the first item in this list */
    public T getFirst(){
        return this.first;
    }
    
    /** Produce a {@link Traversal} for the rest of this list */
    public Traversal<T> getRest(){
        return this.rest;
    }
}
