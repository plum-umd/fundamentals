import tester.Traversal;

/** Represents a list of T */
public interface IList<T> extends Traversal<T>{
    
    /** Is this list empty? */
    public boolean isEmpty();  
    
    /** Produce the first item in this list */
    public T getFirst();

    /** Produce a {@link Traversal} for the rest of this list */
    public Traversal<T> getRest();
}


