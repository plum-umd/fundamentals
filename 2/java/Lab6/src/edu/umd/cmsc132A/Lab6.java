// Authors: partner1, partner2
// Lab 6

package edu.umd.cmsc132A;

import tester.Tester;
import javalib.worldimages.*;
import javalib.funworld.*;
import java.awt.Color;
import java.util.Random;

public class Lab6 { /* Intentionally blank; leave blank */ }

// An interface for simple Balls
interface IBall {
    WorldImage draw(); // draw an image of the ball
    Posn location();   // create a posn of the ball's location
    IBall next();      // create the ball from the next TICKRATE
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
        return null;
    }

    // Return a posn location of this ball
    public Posn location() {
        return new Posn(0, 0);
    }

    // Check if the ball is outside the BallWorld scene
    // Hint: check the static fields in BallWorld for bounds
    Boolean outside() {
        return false;
    }

    // Return the next ball
    // There should be no next ball if this ball is `outside'
    // Otherwise, the ball should move and accelerate down at 9.8 px/s
    // Hint: the acceleration will need to account for the TICKRATE
    // Hint: you may have numeric type problems; use (int) Math.round(...)
    //       to get around those issues, but watch the order of operations
    public IBall next() {
        return new NoBall();
    }
}

class NoBall implements IBall {

    // No ball is drawn as an empty circle (of radius 0)
    public WorldImage draw() {
        return new CircleImage(0, OutlineMode.SOLID, Color.WHITE);
    }

    // No ball's location doesn't matter, so we give 0, 0
    public Posn location() {
        return new Posn(0, 0);
    }

    // The next ball with no ball is no ball
    public IBall next() {
        return this;
    }
}

// The ball world contains some static parameters and a single IBall
class BallWorld extends World {
    static Random r = new Random();  // the random number generator
    static Integer WIDTH = 640;      // the width of the scene
    static Integer HEIGHT = 480;     // the height of the scene
    static Double TICKRATE = 0.1;    // the tick rate (seconds per tick)

    IBall ball;  // the ball of this world

    BallWorld() {
        this(new NoBall());
    }

    BallWorld(IBall ball) {
        this.ball = ball;
    }

    // Create a world with the ball in the given location with random velocity
    // Hint: r.nextInt(10) will return a random number between 0 and 10
    public World onMouseClicked(Posn mouse) {
        return this;
    }

    // Tick this world's ball by calling its `next' method
    public World onTick() {
        return this;
    }

    // Place the ball's image on the scene.
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
