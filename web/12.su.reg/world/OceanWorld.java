import tester.*;
import funworld.*;
import geometry.*;
import colors.*;

import java.awt.*;

// CS 2510 Lab 5: Designing World Games
// 7 February 2012

// A Shark is a new Shark(Integer, Integer)
// Interp: new Shark(y) represents shark at height y in graphics coordinates
class Shark {
  Integer y;
  Integer health;
  WorldImage sharkImage;

  Shark(Integer y, Integer health) {
    this.y = y;
    this.health = health;
    this.sharkImage = new FromFileImage(new CartPt(30, y), "shark.png");
  }

  // This shark chomps the fish
  public Shark chomp(Fish f) {
    // this.y this.health f.p f.health
    return new Shark(this.y, this.health + f.health);
  } 
  
  //move this shark up or down depending of the key
  public Shark onKey(String ke){
    // move this shark up or down depending of the key
    return this;
  }

}

// A Fish swims in the ocean and provides some food value for the shark
class Fish {
  CartPt p;          // the current osition of this fish
  Integer health;    // the food value of this fish for the shark
  
  // make a new fish
  Fish(CartPt p, Integer health) {
    this.p = p;
    this.health = health;
  }

  // Move this fish left.
  public Fish moveLeft() {
    return new Fish(this.p.moveLeft(3), this.health);
  }

  // Is this fish close to the given shark?
  /*
    public Boolean closeByHuh(Shark that) {
    ...
    }
   */
  
  // produce an image of this fish at this fish's location
  WorldImage makeFishImage(){
    return new FromFileImage(this.p, "pink-fish.png");
  }

}

// a CartPt extends the Posn class that represents the (x,y) 
// in graphics coordinates and allows for adding new methods
class CartPt extends Posn {
  
  // the super constructor does the job
  CartPt(Integer x, Integer y) {
    super(x, y);
  }

  // Move this posn left dx units.
  public CartPt moveLeft(Integer dx) {
    return new CartPt(this.x-dx, this.y);
  }
}

// A School is one of:
// - new EmptySchool()
// - new ConsSchool(Fish, School)

// to represent a school of fish
interface School {

  // produce an image of this fishies at this their locations
  public WorldImage makeFishiesImage();
}

// to represent an empty school of fish (no fish)
class EmptySchool implements School {
  EmptySchool() {}

  // produce an image of this fishies at this their locations
  public WorldImage makeFishiesImage(){
    return new RectangleImage(new Posn(0, 0), 0, 0, new White());
  }
}

// to represent a school with at least one fish
class ConsSchool implements School {
  Fish first;
  School rest; 
  
  ConsSchool (Fish first, School rest) {
    this.first = first;
    this.rest = rest;
  }

  // produce an image of this fishies at this their locations
  public WorldImage makeFishiesImage(){
    return this.first.makeFishImage().overlayImages(
           this.rest.makeFishiesImage());
  }
}

// the world of a game where the fish swim left to right and the shark on the 
// right is trying to eat as many as possible to keep alive
class OceanWorld extends World{
  int WIDTH = 600;   // constant values, never change
  int HEIGHT = 300;  // constant values, never change
  Shark shark;       // the shark in the game
  School fishies;    // the fish in the game
  
  OceanWorld(Shark shark, School fishies){
    this.shark = shark;
    this.fishies = fishies;
  }
  
  // produce the image of this world with the shark and fishies...
  public WorldImage makeImage(){
    return
    new RectangleImage(new Posn(300, 150), this.WIDTH, this.HEIGHT, Color.blue).
    overlayImages(this.shark.sharkImage, 
                  this.fishies.makeFishiesImage());
  }

    public World onTick() {
	System.out.println("tick");
	return new OceanWorld(this.shark, this.fishies);
    }
}

// examples and tests for the ocean world
class ExamplesOceanWorld {
  ExamplesOceanWorld() {}

  CartPt p = new CartPt(100, 200);
  Fish f = new Fish(p, 5);
  Shark shark = new Shark(50, 40);
  
  School nofish = new EmptySchool();
  School onefish = new ConsSchool(this.f, this.nofish);
  
  OceanWorld ow = new OceanWorld(this.shark, this.onefish);

  // test the method moveLeft for the CartPt class
  public boolean testMoveLeftCartPt(Tester t){
    return t.checkExpect(new CartPt(100,0).moveLeft(5), new CartPt(95,0));
  }

  // test the method moveLeft for the Fish class
  public boolean testMoveLeftFish(Tester t){
    return t.checkExpect(this.f.moveLeft(), 
        new Fish(new CartPt(97,200), this.f.health));
  }

  // test the method chomp for the Shark class
  public boolean testChompShark(Tester t){
    return t.checkExpect(this.shark.chomp(this.f), 
        new Shark(this.shark.y, this.shark.health + this.f.health));
  }
  
  // run this world at the tick rate 0.1
  public boolean testRunWorld(Tester t){
    return this.ow.bigBang(600, 300, 0.1);
  }
  
}



