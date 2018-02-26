// Authors: partner1, partner2
// Lab 7

package edu.umd.cmsc132A;

import tester.Tester;
import javalib.worldimages.*;
import javalib.funworld.*;
import java.awt.Color;
import java.util.Random;

public class Lab8 { /* Intentionally blank; leave blank */ }

// An interface for simple Balls
interface IBall {
    WorldImage draw();  // draw an image of the ball
    Posn location();    // create a posn of the ball's location
    IBall next();       // create the ball from the next TICKRATE
    Boolean outside();  // is this ball outside the scene?
    Boolean isClicked(Posn mouse);  // was this ball clicked by the mouse?
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
    public Boolean outside() {
        return this.x > BallWorld.WIDTH ||
                this.y > BallWorld.HEIGHT ||
                this.x < 0 ||
                this.y < 0;
    }

    // Return the next ball. The ball should bounce off walls if it hits the
    // boundry, rather than remaining unchanged as currently implemented.
    public Ball next() {
        if (outside()) {
            return this; // Change this!
        } else {
            return new Ball(this.x + this.vx,
                    this.y + this.vy,
                    this.vx,
                    (int) Math.round(9.8 * BallWorld.TICKRATE + this.vy));
        }
    }

    // Returns true only if this ball was clicked with the given mouse event.
    public Boolean isClicked(Posn mouse) {
        return false; // Change this!
    }
}

// An interface for lists of balls
interface ListOfBall {
    ListOfBall next();                   // tick all the balls in the list
    WorldScene draw(WorldScene scene);   // draw all the balls on the given scene
    ListOfBall removeClicked(Posn mouse); // remove all balls clicked by mouse
}

class EmptyLoB implements ListOfBall {
    EmptyLoB(){}

    public ListOfBall next() {
        return new EmptyLoB();
    }

    public WorldScene draw(WorldScene scene) {
        return scene;
    }

    public ListOfBall removeClicked(Posn mouse) {
        return this;
    }
}

class ConsLoB implements ListOfBall {
    Ball first;
    ListOfBall rest;

    ConsLoB(Ball first, ListOfBall rest) {
        this.first = first;
        this.rest = rest;
    }

    // Get the next list of balls. Make sure you cull any balls
    // `outside' the scene from the list.
    public ListOfBall next() {
        return new ConsLoB(first.next(), rest.next());
    }

    public WorldScene draw(WorldScene scene) {
        Posn loc = first.location();
        return rest.draw(scene).placeImageXY(first.draw(), loc.x, loc.y);
    }

    // Implement this!
    public ListOfBall removeClicked(Posn mouse) {
        return this;
    }
}


// The ball world contains some static parameters and ListOfBall
class BallWorld extends World {
    static Random r = new Random();  // the random number generator
    static Integer NBALLOON = 99;    // how many balloons on the scene
    static Integer WIDTH = 640;      // the width of the scene
    static Integer HEIGHT = 480;     // the height of the scene
    static Double TICKRATE = 0.1;    // the tick rate (seconds per tick)

    ListOfBall balls;  // the balls of this world

    static ListOfBall generateBalls(Integer howMany, ListOfBall acc) {
        if (howMany > 0) {
            return generateBalls(howMany - 1,
                    new ConsLoB(new Ball(r.nextInt(WIDTH),
                                         r.nextInt(HEIGHT),
                                         r.nextInt(20) - 10,
                                         r.nextInt(20) - 10),
                                acc));
        } else {
            return acc;
        }
    }

    BallWorld() {
        this(generateBalls(NBALLOON, new EmptyLoB()));
    }

    BallWorld(ListOfBall balls) {
        this.balls = balls;
    }

    // Identify the balls that should be removed from the given mouse click, and remove them.
    public World onMouseClicked(Posn mouse) {
        return this; // Change this!
    }

    public World onTick() {
        return new BallWorld(this.balls.next());
    }

    public WorldScene makeScene() {
        return balls.draw(new WorldScene(this.WIDTH, this.HEIGHT));
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
