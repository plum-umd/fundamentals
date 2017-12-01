
/** Represents a predicate for selecting Balloons smaller 
 *   than a given radius */
public class SmallBalloon implements ISelect<Balloon>{

    /** The (upper) limit for selected Balloons */
    int limit;

    SmallBalloon(int limit){
        this.limit = limit;
    }
    
    /** Is the given Balloon smaller than the limit? */
    public boolean select(Balloon balloon){
        return balloon.radius < this.limit;
    }
}
