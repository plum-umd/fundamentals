import java.awt.Color;

import draw.*;
import geometry.*;
import colors.*;

import image.*;

//------------------------------------------------------------------------------
//represent the world of a Worm Game: Worm, Food, and the Bounding Box
public class WormWorldImage extends World {

  protected WormImage w;
  protected FoodImage f;
  protected BoxImage  b;

  public WormWorldImage(WormImage w, FoodImage f, BoxImage b) {
    this.w = w;
    this.f = f;
    this.b = b;
  }

  // what happens when the clock ticks
  public World onTick() {
    //System.out.println("Tick: (" + this.w.head.x + ", " + this.w.head.y + ")");
    return this.move(this.w.move(this.w.whereTo()));
  }

  // what happens when the player presses a key
  public World onKeyEvent(String ke) {
    //System.out.println("Key event " + ke);
    //System.out.println("Worm at (" + this.w.head.x + ", " + this.w.head.y + ")");
    return this.move(this.w.move(ke));
  }

  // what happens when the player presses a key
  public World onMouseClicked(Posn loc) {
    //System.out.println("Worm at (" + this.w.head.x + ", " + this.w.head.y + ")");
    //System.out.println("Mouse click at (" + loc.x + ", " + loc.y + ")");
    return this.moveFood(loc);
  }
  
  // keep the worm, but move the food to the new location
  protected World moveFood(Posn loc){
    return new WormWorldImage(
        this.w,
        new FoodImage(loc.x, loc.y),
        this.b);
  }
  
  public WorldEnd worldEnds(){
 // worm ate itself
    if (this.w.ateItself()){
      return new WorldEnd(true, this.lastScene("Your worm ate itself."));
    }

    // worm ran into the wall   
    else if (this.w.ranIntoWall(this.b)){
      return new WorldEnd(true, this.lastScene("Your worm ran into a wall."));
    }
    else
      return new WorldEnd(false, this.toDraw());
  }

  // see where the worm moved to - and proceed with the game 
  protected World move(WormImage newWorm) {
 
    // worm found food
    //else 
      if (newWorm.canEat(this.f)){
      return new WormWorldImage(
          this.w = newWorm.eat(this.f),
          this.f.create(this.b),
          this.b);
    }

    // worm just moves ahead
    else // move the worm
      return new WormWorldImage(
          newWorm,
          this.f,
          this.b);
  }

  // draw this world
  public boolean draw() {
    return this.b.draw(this.theCanvas) &&
    this.w.draw(this.theCanvas) &&
    this.f.draw(this.theCanvas);
  }
  
  public Scene toDraw(){
    return
    this.w.drawWormScene(
        this.b.boxScene.placeImage(
            this.f.foodImage, new Posn(this.f.x, this.f.y)));
    
  }
  
  protected Scene lastScene(String message){
    return this.toDraw().
    placeImage(new Text(message, 15, Color.red), new Posn(100, 40));
  }

  public static void main(String[] argv){
    WormWorldImage ww = new WormWorldImage(
        new WormImage(new SegmentImage(50, 60, "up"),
            new MtSegmentImage()), 
            new FoodImage(20, 20), 
            new BoxImage(200, 200));

    ww.bigBang(200, 200, 0.5);

  }

}

//-----------------------------------------------------------------------------
//Worm is a head segment plus a potentially empty sequence of segments

class WormImage {
  SegmentImage head;
  ListSegmentImage body;

  public WormImage(SegmentImage head, ListSegmentImage body) {
    this.head = head;
    this.body = body;
  }

  // where is the head of the worm?
  protected Posn posn() {
    return new Posn(this.head.x,this.head.y);
  }

  // where is this going?
  protected String whereTo() {
    return this.head.whereTo();
  }

  // did this worm run into the wall?
  protected boolean ranIntoWall(BoxImage b) {
    return !b.inside(this.posn());
  }

  // move this worm in the direction of the keyevent
  //   if ke is in left, right, up, down
  // head moves in the direction of the predecessor
  // body element n+1 moves into the position of element n
  // body element 0 moves into the position of head
  protected WormImage move(String ke) {
    if (this.head.move(ke).equal(this.head))
      return this; // the key was not a directional change
    else
      return new WormImage(this.head.move(ke),this.body.move(this.head));
  }

  // did this worm run into itself after a move?
  protected boolean ateItself() {
    return this.body.overlap(this.head);
  }

  // can the head of this worm eat the food?
  protected boolean canEat(FoodImage f) {
    return this.head.canEat(f);
  }

  // move the head into the direction into which it is going already
  // the old head becomes part of the body
  // @pre: canEat(f)
  // @post: !ateItself()
  protected WormImage eat(FoodImage f) {
    return new WormImage(this.head.move(this.whereTo()),
        new ConsSegmentImage(this.head,this.body));
  }

  // draw this worm into c
  protected boolean draw(Canvas c) {
    return this.head.draw(c) &&
    this.body.draw(c);
  }
  // add this food's image to the given scene
  public Scene drawWormScene(Scene scene){
    return
    this.body.drawSegments(this.head.drawSegmentScene(scene));
  }
}


//-----------------------------------------------------------------------------
//represents a segment of the worm, represented as solid disk located at (x,y) 
class SegmentImage {
  int x;
  int y;
  String direction;

  int radius = 5;
  IColor color = new Red();
  int DX;
  int DY;
  Image segmentImage = new Circle(this.radius, "solid", this.color);

  public SegmentImage(int x, int y, String d) {
    this.x = x;
    this.y = y;
    this.DX = 2 * this.radius;
    this.DY = 2 * this.radius;
    this.direction = d;
  }

  // where is it?
  protected Posn posn() {
    return new Posn(this.x,this.y);
  }

  // where is it going?
  protected String whereTo() {
    return this.direction;
  }

  // is this segment equal to that?
  protected boolean equal(SegmentImage that) {
    return this.x == that.x && this.y == that.y && this.direction == that.direction;
  }

  // move in the direction of ke as specified if ke is in left, right, up, down
  protected SegmentImage move(String ke) {
    if (ke.equals("left"))
      return new SegmentImage(this.x-this.DX,this.y,ke);
    else if (ke.equals("right"))
      return new SegmentImage(this.x+this.DX,this.y,ke);
    else if (ke.equals("up"))
      return new SegmentImage(this.x,this.y-this.DY,ke);
    else if (ke.equals("down"))
      return new SegmentImage(this.x,this.y+this.DY,ke);
    else // don't move
      return this;
  }

  // does _this_ segment overlap with segment s
  protected boolean overlap(SegmentImage s) {
    return this.distance(this.x-s.x,this.y-s.y) < 2 * this.radius;
  }

  // can this (head) segment eat the food if it moves in the direction of ke
  protected boolean canEat(FoodImage f) {
    // is there an overlap of the two geometric shapes
    // approximation: is any of the four corners of the food inside s?
    return f.inside(this.posn());
  }

  // draw a worm segment
  protected boolean draw(Canvas c) {
    return c.drawDisk(new Posn(this.x,this.y),this.radius,this.color);
  }
  
  // draw this segment in the given scene
  protected Scene drawSegmentScene(Scene scene){
    return scene.placeImage(this.segmentImage, new Posn(x, y));
  }

  // --- auxiliaries that are only needed for this class: motivate private ---

  // is the Posn inside the disk represented by this segment
  private boolean inside(Posn p) {
    return this.distance(this.x-p.x,this.y-p.y) <= this.radius;
  }

  // how far is (x,y) from the origin
  private double distance(int x, int y) {
    return Math.sqrt(x*x+y*y);
  }
}


//-----------------------------------------------------------------------------
//a list of worm segments that represent the worm's body
//composite pattern: it is a composite because every segment behaves the same way
class MtSegmentImage extends ListSegmentImage {
  public MtSegmentImage() {}

  protected boolean overlap(SegmentImage s) { return false; }

  protected ListSegmentImage move(SegmentImage pred) { 
    return new MtSegmentImage(); }

  protected boolean draw(Canvas c) { return true; }
  
  // draw all segments onto the given scene
  protected Scene drawSegments(Scene scene){
    return scene;
  }
}

//-----------------------------------------------------------------------------
//a list of worm segments that represent the worm's body
//composite pattern: it is a composite because every segment behaves the same way
class ConsSegmentImage extends ListSegmentImage {
  SegmentImage first;
  ListSegmentImage rest;

  public ConsSegmentImage(SegmentImage first, ListSegmentImage rest) {
    this.first = first;
    this.rest = rest;
  }

  protected boolean overlap(SegmentImage s) {
    return this.first.overlap(s) || this.rest.overlap(s);
  }

  protected ListSegmentImage move(SegmentImage pred) {
    return new ConsSegmentImage(pred,this.rest.move(this.first));
  }

  protected boolean draw(Canvas c) {
    return this.first.draw(c) &&
    this.rest.draw(c);
  }
  
  // draw all segments onto the given scene
  protected Scene drawSegments(Scene scene){
    return this.rest.drawSegments(this.first.drawSegmentScene(scene));
  }

}



//-----------------------------------------------------------------------------
//a list of worm segments that represent the worm's body
//composite pattern: it is a composite because every segment behaves the same way
abstract class ListSegmentImage {

  // does s overlap with any of the segments in _this_ body
  protected abstract boolean overlap(SegmentImage s);

  // move each segment in this list of segments into the position of its
  // predecessor, given the new position for the predecessor (head originally)
  protected abstract ListSegmentImage move(SegmentImage pred);

  // draw the list of segments on c
  protected abstract boolean draw(Canvas c);
  
  // draw all segments onto the given scene
  protected abstract Scene drawSegments(Scene scene);
}


//-----------------------------------------------------------------------------
//represents a piece of food as a solid rectangle with northwest corner at (x,y)
class FoodImage {
  int x;
  int y;

  int width = 20;
  int height = 10;
  IColor color = new Green();
  public Image foodImage = 
    new RectangleImage(this.width, this.height, "solid", this.color);

  public FoodImage(int x, int y) {
    this.x = x;
    this.y = y;
  }

  // is p inside of this Food shape
  protected boolean inside(Posn p) {
    return
    (this.x <= p.x && p.x <= this.x+this.width) &&
    (this.y <= p.y && p.y <= this.y+this.width);
  }

  // create a food pile that is at a different location than this (trial and error)
  protected FoodImage create(BoxImage b) {
    return this.createAux(
        b,
        (this.x + 22) % (b.width - 20),
        (this.y + 33) % (b.height - 30));
  }

  // make sure the new food is different from the last one
  protected FoodImage createAux(BoxImage b, int x, int y) {
    if ((this.x != x) && this.y != y)
      return new FoodImage(x,y);
    else
      return this.create(b);
  }
  
  // add this food's image to the given scene
  public Scene drawFoodScene(Scene scene){
    return scene.placeImage(this.foodImage, new Posn(this.x, this.y));
  }

  // draw this food
  public boolean draw(Canvas c) {
    return c.drawRect(new Posn(this.x,this.y),this.width,this.height,this.color);
  }
}

//-----------------------------------------------------------------------------
//Box: represents the playing field for the worm game

class BoxImage {
  int width;
  int height;
  Scene boxScene;

  protected BoxImage(int width, int height) {
    this.width = width;
    this.height = height; 
    this.boxScene = new EmptyScene(this.width, this.height, new Blue());
  }

  // is p inside of _this_ box?
  protected boolean inside(Posn p) {
    return
    (0 <= p.x && p.x <= this.width) &&
    (0 <= p.y && p.y <= this.height);
  }

  // paint the background blue, so it is shown when applet runs
  protected boolean draw(Canvas canvas){
    return canvas.drawRect(new Posn(0, 0), width, height, new Blue());
  }
}

