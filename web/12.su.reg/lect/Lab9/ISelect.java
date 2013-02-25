
/** Represents a generic predicate */
public interface ISelect<T>{
    
    /** Should the given thing be selected? */
    public boolean select(T t);
}
