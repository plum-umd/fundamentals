// Author: directoryID
// Assignment 4

package edu.umd.cmsc132A;

import tester.Tester;
import javalib.funworld.*;
import javalib.worldimages.*;
import java.awt.Color; // Use Color.black, Color.red, etc.

public class Assign4 {
    // Constants for the scene width and height
    public static Integer WIDTH  = 300;
    public static Integer HEIGHT = 300;

    // Use the "BigBang" configuration to run this game
    public static void main(String[] args) {
        new Game().bigBang(Assign4.WIDTH, Assign4.HEIGHT);
    }
}

// Representation of the Space invaders game state
class Game extends World {

    public WorldScene makeScene() {
        // Change to actually visualize the state of the game
        return new WorldScene(Assign4.WIDTH, Assign4.HEIGHT)
                    .placeImageXY(new TextImage("Space Invaders", 12., Color.black),
                            Assign4.WIDTH / 2,
                            Assign4.HEIGHT / 2);
    }

    public Game onTick() {
        // Change to actually advance the state of the game
        return this;
    }

    public Game onKeyEvent(String key) {
        // Change to actually respond to key events
        return this;
    }
}

class Tests {
    // Write test cases here, run "Test" configuration to test
    Boolean testObvious(Tester t) {
        return t.checkExpect(true, true);
    }
}
