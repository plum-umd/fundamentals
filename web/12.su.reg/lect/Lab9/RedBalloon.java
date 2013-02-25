
/** Represents a predicate that selects red Balloons */
public class RedBalloon implements ISelect<Balloon>{

    /** Is the given Balloon red? */
    public boolean select(Balloon balloon){
        return balloon.color.equals("red");
    }
}
