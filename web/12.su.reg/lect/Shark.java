import tester.*;
  
/* --- THE OCEAN WORLD -----------------------------------------------
Fish swim across the screen from right to left. There may be one fish,
or a whole school of fish, or none at a time. A hungry shark swims in
place at the left side, moving only up and down, controlled by the
\scheme{"up"} and \scheme{"down"} keys. It tries to catch and eat the
fish. It gets bigger with each fish it eats, it keeps getting hungrier
and hungrier as the time goes on between the catches. It dies of
starvation, if not fed in time.
--------------------------------------------------------------------*/

import colors.*;
import draw.*;
import geometry.*;

import image.*;

import java.util.Random;
import java.awt.Color;

/*
+----------+
| Shark    |
+----------+
| int y    |
| int life |
+----------+

*/

// to represent a shark in an ocean world
class Shark {
  int y;
  int life;

  //Image sharkPic = new FromFile("shark.png");
  
  
  Shark(int y, int life) {
    this.y = y;
    this.life = life;
  }
  
  // produce a new shark from this shark moved in response to the key press
  Shark onKeyEvent(String ke){
    if (ke.equals("up"))
      return new Shark(this.y - 3, this.life);
    else if (ke.equals("down"))
      return new Shark(this.y + 3, this.life);
    else
      return this;
  }
  
  // draw this shark on the given Canvas
  boolean draw(Canvas c){
    return c.drawDisk(new Posn(15, this.y), this.life / 10 + 2, new Black()) &&
           c.drawLine(new Posn(15, this.y), 
                      new Posn(40, this.y), 
                      new Black());
  }
  
  // the image of this shark, its size depends on its life health
  Image sharkImage(){
    return 
    //this.sharkPic.overlay(
        new Circle(this.life / 10 + 2, "solid", new Black());//);
  }
  
//the image of this shark, its size depends on its life health
  Scene drawShark(Scene scene){
    //return scene.placeImage(this.sharkImage(), new Posn(15, this.y));
    return scene.placeImage(this.sharkImage(), 
        new Posn(15, this.y));
  }
  
  // produce a shark hungrier by a minute than this one
  Shark onTick(){
    if (this.life > 0)
      return new Shark(this.y, this.life - 1);
    else
      return this;
  }
    
  // produce a shark after this one ate one fish
  // for now every fish tastes the same
  // later, fish nutrition value may be determined by the size or the color 
  Shark getFatter(){
    return new Shark(this.y, this.life + 30);
  }
  
  // is this shark dead?
  boolean isDead(){
    return this.life <= 0;
  }
  
}

/*
+-----------------+
| Fish            |
+-----------------+
| Posn loc        |
| boolean escaped |
+-----------------+

*/

// to represent a fish in an ocean world
class Fish {
  Posn loc;
  boolean hasEscaped;
  Image fishImage = new Circle(10, "solid", new Red());
    
  static Random rand = new Random();

  Fish(Posn loc, boolean hasEscaped) {
    this.loc = loc;
    this.hasEscaped = hasEscaped;
  }
  
  // produce from this fish the one that swam for a minute more
  Fish onTick(){
    if (hasEscaped)
      return this.makeFish(200, 200);
    else if (this.loc.x < 0)
      return new Fish(this.loc, true);
    else{
      return new Fish(new Posn(this.loc.x - 3, 
                               this.loc.y + rand.nextInt(7)  - 3),
                               false);
    }
  }
  
  // has this fish escaped?
  boolean escaped(){
    return (this.loc.x < 0) || this.hasEscaped;
  }
  
  // is this fish a food for the given shark?
  boolean isFood(Shark shark){
    return
      Math.sqrt((this.loc.x - 15) * (this.loc.x - 15) +
                (this.loc.y - shark.y) * (this.loc.y - shark.y)) < 25;     
  }

  // draw this fish on the given Canvas
  boolean draw(Canvas c){
    return c.drawDisk(this.loc, 10, new Red());
  }
  
  // the image of this shark, its size depends on its life health
  Scene drawFish(Scene scene){
    return scene.placeImage(this.fishImage, this.loc);
  }
  
  // produce a new fish at the right quarter of the canvas at random height
  Fish makeFish(int WIDTH, int HEIGHT){
    return new Fish(new Posn(WIDTH - WIDTH / 4 + rand.nextInt(WIDTH / 4), 
                             5 + rand.nextInt(HEIGHT - 10)), false);
  }
  
  // replace this fish with a new one
  Fish feedShark(Shark shark){
    return this.makeFish(200, 200); 
  }
}

/*
+-------------+
| OceanWorld  |
+-------------+
| Shark shark |
| Fish fish   |
+-------------+

*/


// to represent an ocean world
class OceanWorld extends World{
  Shark shark;
  Fish fish;
  int WIDTH =  200;
  int HEIGHT = 200;
  Scene BACKGROUND = new EmptyScene(WIDTH, HEIGHT, new Blue()); 

  OceanWorld(Shark shark, Fish fish) {
    this.shark = shark;
    this.fish = fish;
  }
  
  // start the world and the timer
  boolean go() { return this.bigBang(200, 200, 0.05); }

  // produce a new OceanWorld after one minute elapsed: 
  // move the fish, starve the shark, t.checkExpect(if the fish is eaten or has escaped, 
  public World onTick(){
    // if the shark found fish, fed the shark, replace the fish with a new one                        
    if (this.fish.isFood(this.shark))
      return new OceanWorld(this.shark.getFatter(),
                            this.fish.feedShark(shark));
    
    // no special events, just move the fish and starve the shark 
    else 
      return new OceanWorld(this.shark.onTick(), this.fish.onTick());
  }
  
  public WorldEnd worldEnds(){
    
    // if the shark starved to death, end the world
    if (this.shark.isDead())
      return new WorldEnd(true, 
          this.makeLastScene("The shark starved to death", 
              new Posn(100, 30), Color.red));
    else
      return new WorldEnd(false, this.toDraw());
  }
  
  // produce a new OceanWorld in response to the given key press
  public World onKeyEvent(String ke){
    return new OceanWorld(this.shark.onKeyEvent(ke), this.fish);
  }
  
  // draw this world
  public boolean draw(){
    return this.theCanvas.drawRect(new Posn(0, 0), 
                            this.WIDTH, this.HEIGHT, new Blue()) &&
           this.fish.draw(this.theCanvas) &&
           this.shark.draw(this.theCanvas);
  }
  
  // the image of this shark and the fish in the ocean
  public Scene toDraw(){
    return this.fish.drawFish(this.shark.drawShark(this.BACKGROUND));
  }
 
}

// Examples of the classes Fish, Shark, OceanWorld and tests for all methods
class ExamplesOceanWorld{
  ExamplesOceanWorld(){}
  
  Shark mac = new Shark(100, 200);
  Shark mac2 = new Shark(100, 100);
  Shark mac3 = new Shark(100, -1);
  
  Fish fishy = new Fish(new Posn(190, 100), false);
  Fish yummy = new Fish(new Posn(15, 98), false);
  Fish gone = new Fish(new Posn(15, 98), true);
  Fish gone2 = new Fish(new Posn(-2, 98), false);

  
    
  OceanWorld sfw = new OceanWorld(this.mac, this.fishy); 
  
  Canvas c = new Canvas(200, 200);
  
  
  
  // test new Random().nextInt() % 7
  public void testRandom(Tester t){
    t.checkOneOf(new Random().nextInt() % 2,
        new Integer[]{-2, -1, 0, 1, 2});
  }
  
  // test the method onKeyEvent in the class Shark
  boolean testSharkOnKeyEvent(Tester t){
    return
      t.checkExpect(this.mac.onKeyEvent("left"),  this.mac) && 
      t.checkExpect(this.mac.onKeyEvent("up"),  
        new Shark(97, 200)) && 
      t.checkExpect(this.mac.onKeyEvent("down"),  
        new Shark(103, 200));
  }
  
  // a visual test for drawing a Shark
  boolean drawShark(){
    return c.show() && this.mac.draw(c);
  }
  
  // test the method onTick in the class Shark
  boolean testSharkOnTick(Tester t){
    return
      t.checkExpect(this.mac.onTick(), 
        new Shark(100, 199)) && 
      t.checkExpect((new Shark(100, 0)).onTick() , 
        new Shark(100, 0)); 
  }
  
  // test the method getFatter in the class Shark
  boolean testGetFatter(Tester t){
    return t.checkExpect(mac.getFatter(), 
             new Shark(100, 230)) &&
           t.checkExpect(mac2.getFatter(), 
             new Shark(100, 130));
  }

  // test the method isDead in the class Shark
  boolean testisDead(Tester t){
    return t.checkExpect(mac.isDead(),  false) &&
           t.checkExpect(mac3.isDead(),  true);
  }


  // test the method onTick in the class Fish
  // results are correct if the random move is replaced with exactly 5
  boolean testFishOnTick(Tester t){
    t.checkOneOf(this.fishy.onTick(), 
          new Fish[]{
              new Fish(new Posn(187, 97), false),
              new Fish(new Posn(187, 98), false),
              new Fish(new Posn(187, 99), false),
              new Fish(new Posn(187, 100), false),
              new Fish(new Posn(187, 101), false),
              new Fish(new Posn(187, 102), false),
              new Fish(new Posn(187, 103), false),
              }, "Fish ontick");
   return 
   t.checkExpect((new Fish(new Posn(-2, 100), true)).onTick() , 
        new Fish(new Posn(-2, 100), true)) && 
   t.checkExpect((new Fish(new Posn(-2, 100), false)).onTick() , 
        new Fish(new Posn(-2, 100), true)); 
  }
  
  // test the method escaped in the class Fish
  boolean testFishEscaped(Tester t){
    return 
      t.checkExpect(this.fishy.escaped(),  false) && 
      t.checkExpect((new Fish(new Posn(-2, 100), true)).escaped(),  true) &&
      t.checkExpect((new Fish(new Posn(-2, 100), false)).escaped(),  true); 
  }

  // test the method isFood in the class Fish
  boolean testFishIsFood(Tester t){
    return 
      t.checkExpect(this.fishy.isFood(this.mac),  false) &&
      t.checkExpect(this.yummy.isFood(new Shark(100, 20)),  true);
  } 

  // a visual test for drawing a Fish
  boolean drawFish(){
    return c.show() && this.fishy.draw(c);
  }

  // test the method makeFish in the class Fish
  boolean testMakeFish(Tester t){
    return t.checkExpect(this.fishy.makeFish(200, 200).hasEscaped,  false) &&
           t.checkExpect(this.fishy.makeFish(200, 200).loc.y < 200,  true) &&
           t.checkExpect(this.fishy.makeFish(200, 200).loc.y >   0,  true) &&
           t.checkExpect(this.fishy.makeFish(200, 200).loc.x < 200,  true) &&
           t.checkExpect(this.fishy.makeFish(200, 200).loc.x > 150,  true);
  }
  

  // test the method onKeyEvent in the class OceanWorld
  boolean testOceanOnKeyEvent(Tester t){
    return
      t.checkExpect(this.sfw.onKeyEvent("left") , 
        new OceanWorld(this.mac, this.fishy)) && 
      t.checkExpect(this.sfw.onKeyEvent("up"),  
        new OceanWorld(new Shark(97, 200), this.fishy)) && 
      t.checkExpect(this.sfw.onKeyEvent("down"),  
        new OceanWorld(new Shark(103, 200), this.fishy));
  }
  
  OceanWorld sfwEnd = new OceanWorld(this.mac3, this.fishy);
  OceanWorld sfwReg = new OceanWorld(this.mac, this.fishy);
  OceanWorld sfwYum = new OceanWorld(this.mac, this.fishy);
  
  // test the method onTick in the class OceanWorld
  boolean testOceanOnTick(Tester t){
    return
      // world goes on
      this.sfwReg.bigBang(300, 300, 0.1) &&
      t.checkExpect(this.sfwReg.onTick() , 
        new OceanWorld(this.mac.onTick(), this.fishy.onTick())) &&

      // fish is eaten, shark is fatter, new fish appears
      t.checkExpect(this.sfwYum.onTick() , 
        new OceanWorld(this.mac.getFatter(), 
                              this.fishy.makeFish(200, 200)));

      // world ends - the shark starves to death --- fails due to randomness
      // cannot invoke method in the teachpack - no test is possible
      //t.checkExpect(this.sfwEnd.onTick() , 
      //  this.sfw.endWorld("The shark has starved to death"));
    
  }
 
  public static void main(String[] argv){
  // construct an instance of the OceanWorld
  Shark mac = new Shark(100, 200);
  Fish fishy = new Fish(new Posn(190, 100), false);
  OceanWorld sfw = new OceanWorld(mac, fishy); 

  // and run the OceanWorld
  sfw.bigBang(200, 200, 0.1);
  }
/*
  // a visual test for drawing the OceanWorld
  boolean drawOcean(){
    return this.sfw.draw();
  }
*/

}
