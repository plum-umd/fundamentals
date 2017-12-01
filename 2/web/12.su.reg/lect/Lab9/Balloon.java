
/** Represents a round Balloon */
public class Balloon {
    int x;
    int y;
    int radius;
    String color;
  
    public Balloon (int x, int y, int radius, String color){
        this.x = x;
        this.y = y;
        this.radius = radius;
        this.color = color;
    }
    
    /** Compute the distance of this balloon from the top of window */
    int distanceFromTop(){
        return this.y - this.radius;
    } 
}
