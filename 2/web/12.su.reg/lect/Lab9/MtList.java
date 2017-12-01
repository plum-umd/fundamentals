import tester.*;

/** Represents an empty list of T */
public class MtList<T> implements IList<T>{

    public MtList(){}

    /** Is this list empty? */
    public boolean isEmpty(){ return true; }  

    /** Produce the first item in this list */
    public T getFirst(){
        throw new IllegalUseOfTraversalException("No first element");
    }
  
    /** Produce a {@link Traversal} for the rest of this list */
    public Traversal<T> getRest(){
        throw new IllegalUseOfTraversalException("No more elements");
    }
}       
