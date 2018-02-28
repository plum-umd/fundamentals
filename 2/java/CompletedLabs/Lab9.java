// Authors: partner1, partner2
// Lab 9

package edu.umd.cmsc132A;

import tester.Tester;
import javalib.worldimages.*;
import javalib.funworld.*;
import java.awt.Color;
import java.util.function.*;
import java.util.Random;
import java.util.function.IntUnaryOperator;

public class Lab9 { /* Intentionally blank; leave blank */ }

// An interface for simple Balls
interface IBall {
  WorldImage draw();  // draw an image of the ball
  Posn location();    // create a posn of the ball's location
  IBall tick();       // create the ball from the next tick of the clock
  IBall jump();     // create the ball after bouncing it up a bit
  Boolean isOutside();  // is this ball past the left or right edge of the scene?
  Boolean isCrashed();  // is this ball colliding with the bottom of the scene?
  Boolean isBounced();  // is this ball bouncing off the ceiling
  Boolean collide(IBall other);  // is this ball colliding with the given ball?
}

// Ball class
class Ball implements IBall {
  static Integer RADIUS = 20;  // the radius of all balls
  static Integer JUMP = 10;  // the amount of velocity added by a jump
  Integer x;   // the X pixel location of this ball
  Integer y;   // the Y pixel location of this ball
  Integer vx;  // the velocity of the ball on the X axis
  Integer vy;  // the velocity of the ball on the Y axis
  Color color;

  // Default color (red) constructor
  Ball(Integer x, Integer y, Integer vx, Integer vy) {
    this(x, y, vx, vy, Color.RED);
  }

  // Arbitrary color constructor
  Ball(Integer x, Integer y, Integer vx, Integer vy, Color color) {
    this.x = x;
    this.y = y;
    this.vx = vx;
    this.vy = vy;
    this.color = color;
  }

  // Draw this ball as a solid, CircleImage
  public WorldImage draw() {
    return new CircleImage(RADIUS, OutlineMode.SOLID, color);
  }

  // Return a posn location of this ball
  public Posn location() {
    return new Posn(this.x, this.y);
  }

  // Has this ball crashed into the bottom of the scene?
  public Boolean isCrashed() {
    return this.y + RADIUS > BallWorld.HEIGHT;
  }

  // Has this ball crashed into the top of the scene?
  public Boolean isBounced() {
    return this.y - RADIUS < 0;
  }

  // Is this ball to the left or right of the scene?
  public Boolean isOutside() {
    return this.x - RADIUS > BallWorld.WIDTH ||
            this.x + RADIUS < 0;
  }

  // Is this ball colliding with the given ball?
  public Boolean collide(IBall other) {
    Posn that = other.location();
    return Math.hypot(this.x - that.x, this.y - that.y) <= 2*RADIUS;
  }

  // Exercise 3:
  private Ball modifyVY(Function<Integer, Integer> f) {
    /* return this; */
    Integer newVY = f.apply(this.vy);
    return new Ball(this.x + this.vx,
            this.y + newVY,
            this.vx,
            newVY,
            this.color);
  }

  // Exercise 3:
  // Return a new ball with its vertical velocity negated
  public Ball bounce() {
    return modifyVY((vy) -> -vy);
    /* Exercise 3:
    Integer vyBounce = -this.vy;
    return new Ball(this.x + this.vx,
                    this.y + vyBounce,
                    this.vx,
                    vyBounce,
                    this.color);
                    */
  }

  // Exercise 3:
  // Return a new ball accelerated upward by JUMP px/tick
  public Ball jump() {
    return modifyVY((vy) -> vy - JUMP);
    /* Exercise 3:
    Integer vyJump = this.vy - JUMP;
    return new Ball(this.x + this.vx,
                    this.y + vyJump,
                    this.vx,
                    vyJump,
                    this.color);
                    */
  }

  // Exercise 3:
  // Return the next ball
  public Ball tick() {
    if (isBounced() || isCrashed()) {
      return this.bounce();
    } else {
      return modifyVY((vy) -> Math.round(9.8f * BallWorld.TICKRATE + vy));
      /* Exercise 3:
      Integer vyGrav = Math.round(9.8f * BallWorld.TICKRATE + vy);
      return new Ball(this.x + this.vx,
              this.y + vyGrav,
              this.vx,
              vyGrav,
              this.color);
              */
    }
  }
}

// An interface for lists of balls
interface ListOfBall {
  ListOfBall tick();                   // tick all the balls in the list
  ListOfBall append(ListOfBall that);  // append that list to the end of this list
  Boolean isEmpty();                   // true only if this list is empty
  Boolean exists(Predicate<IBall> p);   // true if any element of the list satisfies p
  WorldScene draw(WorldScene scene);   // draw all the balls on the given scene
}

class EmptyLoB implements ListOfBall {
  EmptyLoB(){}

  public ListOfBall tick() {
    return new EmptyLoB();
  }

  // Exercise 1
  public ListOfBall append(ListOfBall that) {
    /* return this; */
    return that;
  }

  public Boolean isEmpty() { return true; }

  // Exercise 2:
  public Boolean exists(Predicate<IBall> p) {
    return false;
  }

  public WorldScene draw(WorldScene scene) {
    return scene;
  }
}

class ConsLoB implements ListOfBall {
  IBall first;
  ListOfBall rest;

  ConsLoB(IBall first, ListOfBall rest) {
    this.first = first;
    this.rest = rest;
  }

  // Get the next list of balls.
  public ListOfBall tick() {
    if (this.first.isOutside()) {
      return this.rest.tick();
    } else {
      return new ConsLoB(this.first.tick(), this.rest.tick());
    }
  }

  // Exercise 1:
  public ListOfBall append(ListOfBall that) {
    /* return this; */
    return new ConsLoB(this.first, this.rest.append(that));
  }

  public Boolean isEmpty() {
    return false;
  }

  // Exercise 2:
  public Boolean exists(Predicate<IBall> p) {
    /* return false; */
    return p.test(this.first) || this.rest.exists(p);
  }

  public WorldScene draw(WorldScene scene) {
    Posn p = first.location();
    return rest.draw(scene).placeImageXY(first.draw(), p.x, p.y);
  }
}


// The ball world contains some static parameters and ListOfBall
class BallWorld extends World {
  static Random r = new Random();  // the random number generator
  static Integer WIDTH = 640;      // the width of the scene
  static Integer HEIGHT = 480;     // the height of the scene
  static Integer FONTSIZE = 48;    // the font size of the score
  static Float TICKRATE = 0.1f;    // the tick rate (seconds per tick)

  Integer score;
  IBall bouncy;
  ListOfBall obstacles;  // the balls of this world

  BallWorld() {
    this(0,
            new Ball(WIDTH / 3, HEIGHT / 2, 0, 0, Color.GREEN),
            new EmptyLoB());
  }

  BallWorld(Integer score, IBall bouncy, ListOfBall obstacles) {
    this.score = score;
    this.bouncy = bouncy;
    this.obstacles = obstacles;
  }

  public World onKeyEvent(String key) {
    if (" ".equals(key)) {
      return new BallWorld(score, bouncy.jump(), obstacles);
    } else {
      return this;
    }
  }

  private ListOfBall randomObstacle() {
    if (r.nextInt((int) Math.round(TICKRATE * 100)) == 0) {
      Integer x = WIDTH - Ball.RADIUS;
      Integer y = r.nextInt(HEIGHT - 2*Ball.RADIUS) + Ball.RADIUS;
      Integer vx = r.nextInt(Ball.JUMP) + Ball.JUMP;
      Integer vy = r.nextInt(Ball.JUMP);
      Ball obst = new Ball(x, y, -vx, vy);
      return new ConsLoB(obst, new EmptyLoB());
    } else {
      return new EmptyLoB();
    }
  }

  // Tick this world's balls
  public World onTick() {
    if (bouncy.isCrashed()) {
      return this.endOfWorld("Crashed! Game Over!");
    } else if (obstacles.exists((b) -> bouncy.collide(b))) {
      return this.endOfWorld("Hit! Game Over!");
    } else {
      ListOfBall maybe = randomObstacle();
      Integer newScore = maybe.isEmpty() ? score : score+1;
      return new BallWorld(newScore, bouncy.tick(), maybe.append(obstacles).tick());
    }
  }

  private WorldScene makeScore() {
     WorldImage score =
             new TextImage(Integer.toString(this.score),
                     FONTSIZE,
                     Color.BLUE);
     WorldScene scene = new WorldScene(WIDTH, HEIGHT);
     return scene.placeImageXY(score, WIDTH / 2, HEIGHT / 2);
  }

  // Place the balls on the scene
  public WorldScene makeScene() {
    ListOfBall allBalls = new ConsLoB(bouncy, obstacles);
    return allBalls.draw(makeScore());
  }

  public WorldScene lastScene(String msg) {
    WorldScene scene = makeScene();
    WorldImage gameOver = new TextImage(msg, FONTSIZE, Color.RED);
    return scene.placeImageXY(gameOver, WIDTH / 2, HEIGHT / 3);
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
