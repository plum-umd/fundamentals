// Authors: partner1, partner2
// Lab 7

package edu.umd.cmsc132A;

import tester.Tester;
import javalib.worldimages.*;
import javalib.funworld.*;
import java.awt.Color;
import java.util.Random;

public class Lab7 { /* Intentionally blank; leave blank */ }

// An interface for simple Balls
interface IBall {
  WorldImage draw();  // draw an image of the ball
  Posn location();    // create a posn of the ball's location
  IBall next();       // create the ball from the next TICKRATE
  Boolean outside();  // is this ball outside the scene?
}

// Ball class
class Ball implements IBall {
  static Integer BALLRADIUS = 20;  // the radius of all balls
  Integer x;   // the X pixel location of this ball
  Integer y;   // the Y pixel location of this ball
  Integer vx;  // the velocity of the ball on the X axis
  Integer vy;  // the velocity of the ball on the Y axis

  Ball(Integer x, Integer y, Integer vx, Integer vy) {
    this.x = x;
    this.y = y;
    this.vx = vx;
    this.vy = vy;
  }

  // Draw this ball as a solid, red CircleImage
  public WorldImage draw() {
    return new CircleImage(BALLRADIUS, OutlineMode.SOLID, Color.RED);
  }

  // Return a posn location of this ball
  public Posn location() {
    return new Posn(this.x, this.y);
  }

  // Check if the ball is outside the BallWorld scene
  // Hint: check the static fields in BallWorld for bounds
  public Boolean outside() {
    return this.x - BALLRADIUS > BallWorld.WIDTH ||
      this.y - BALLRADIUS > BallWorld.HEIGHT ||
      this.x + BALLRADIUS < 0 ||
      this.y + BALLRADIUS < 0;
  }

  // Return the next ball
  // The ball should move and accelerate down at 9.8 px/s
  public Ball next() {
    return new Ball(this.x + this.vx,
                    this.y + this.vy,
                    this.vx,
                    (int) Math.round(9.8 * BallWorld.TICKRATE + this.vy));
  }
}

// An interface for lists of balls
interface ListOfBall {
  ListOfBall next();                   // tick all the balls in the list
  WorldScene draw(WorldScene scene);   // draw all the balls on the given scene
}

class EmptyLoB implements ListOfBall {
  EmptyLoB(){}

  public ListOfBall next() {
    return new EmptyLoB();
  }

  public WorldScene draw(WorldScene scene) {
    return scene;
  }
}

class ConsLoB implements ListOfBall {
  // You'll need to modify this constructor
  ConsLoB(){}

  // Get the next list of balls. Make sure you cull any balls
  // `outside' the scene from the list.
  public ListOfBall next() {
    return new EmptyLoB();
  }

  // Draw all the balls in this list on the given scene
  public WorldScene draw(WorldScene scene) {
    return scene;
  }
}


// The ball world contains some static parameters and ListOfBall
class BallWorld extends World {
  static Random r = new Random();  // the random number generator
  static Integer WIDTH = 640;      // the width of the scene
  static Integer HEIGHT = 480;     // the height of the scene
  static Double TICKRATE = 0.1;    // the tick rate (seconds per tick)

  ListOfBall balls;  // the balls of this world

  BallWorld() {
    this(new EmptyLoB());
  }

  BallWorld(ListOfBall balls) {
    this.balls = balls;
  }

  // Create a world with an additional ball in the given location with
  // random velocity.
  // Hint: r.nextInt(10) will return a random number between 0 and 10
  public World onMouseClicked(Posn mouse) {
    return this;
  }

  // Tick this world's balls
  public World onTick() {
    return this;
  }

  // Place the balls on the scene
  public WorldScene makeScene() {
    WorldScene scene = new WorldScene(this.WIDTH, this.HEIGHT);
    return scene;
  }
}

//-----------------------------------------------------------------------------
// Main

// You can ignore this, it just gets bigBang up and running
class Main {
  Boolean testBallWorld(Tester t) {
    BallWorld w = new BallWorld();
    return w.bigBang(BallWorld.WIDTH, BallWorld.HEIGHT, BallWorld.TICKRATE);
  }
}
