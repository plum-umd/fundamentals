

import java.awt.Color;

import funworld.*;
import geometry.*;
import colors.*;


//-----------------------------------------------------------------------------
// A very simple example of a world
public class ExamplesWorld extends World{
  Posn pos;
  boolean worldEnd = false;

  ExamplesWorld(Posn pos, boolean worldEnd){
    this.pos = pos;
    this.worldEnd = worldEnd;
  }
  
  ExamplesWorld(Posn pos){
    this(pos, false);
  }

  // just a red ball moving across the screen
  public WorldImage makeImage(){
    return 
    new OverlayImages(new RectangleImage(this.pos, 60, 20, new Red()),
      new OverlayImages(new TextImage(this.pos, "hello", 12, 0, new Blue()),
        new LineImage(this.pos, new Posn(this.pos.x + 5, this.pos.y - 5), 
            new Black())));
  }

  // test all kinds of actions using key events:
  // left and right keys move the ball in the given direction
  // up key returns the ball to the left edge
  // "E" ends the world
  // "s" invokes endOfTime method
  // "q" throws a runtime exception and quits
  public World onKeyEvent(String ke){
    System.out.println("Key event " + ke);
    if (ke.equals("right"))
      return new ExamplesWorld(new Posn(this.pos.x + 20, this.pos.y));
    if (ke.equals("left"))
      return new ExamplesWorld(new Posn(this.pos.x - 20, this.pos.y));
    if (ke.equals("up"))
      return new ExamplesWorld(new Posn(0, this.pos.y));
    if (ke.equals("E"))
      return new ExamplesWorld(this.pos, true);
    if (ke.equals("q"))
      throw new RuntimeException("We quit!!!");
    return this;
  }

  /**
   * Check whether the World ended.
   */
  public WorldEnd worldEnds (){
    // if the blob is outside the canvas, stop
    if (this.worldEnd)
      return 
      new WorldEnd(true, 
          this.makeLastImage("End of the World!!", 
                             new Posn(100, 40), Color.red));
    else
       return new WorldEnd(false, this.makeImage());
   }
  
  
  // at each tick print the current position of the ball
  public World onTick(){
    System.out.println("Tick -- pos = (" + this.pos.x + ", " + this.pos.y + ")");
    return new ExamplesWorld(new Posn((this.pos.x + 2) % 100, this.pos.y));
  }
  
  // at each mouse click print the location of the mouse click
  public World onMouseClicked(Posn loc){
    System.out.println("Click -- pos = (" + loc.x + ", " + loc.y + ")");
    return new ExamplesWorld(new Posn(loc.x, loc.y));
  }

  // run two worlds concurrently - the one in focus responds to mouse and key
  // events, but the clock ticks regardless
  public static void main(String[] argv){
    ExamplesWorld ew = new ExamplesWorld(new Posn(50, 60));
    ExamplesWorld ew2 = new ExamplesWorld(new Posn(50, 20));

    // runs two worlds: switch between by clicking on the window top bar
    // output coordinates report on the window currently in focus
    // to see that mouse click works correctly, comment out one of the worlds
    ew.bigBang(200, 300, 0.1);
    ew2.bigBang(400, 200, 0.3);

  }

}