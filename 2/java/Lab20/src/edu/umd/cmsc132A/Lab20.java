package edu.umd.cmsc132A;

import javalib.impworld.World;
import javalib.impworld.WorldScene;
import javalib.worldimages.*;
import tester.Tester;

import java.awt.*;
import java.util.Optional;

import static java.awt.Color.*;

public class Lab20 { /* Intentionally blank; leave blank */ }

class Hanoi extends World {

  Integer nRings  = 3; // the number of rings  in this world
  Integer nTowers = 3; // the number of towers in this world
  Integer width;       // the pixel width  of the scene
  Integer height;      // the pixel height of the scene

  // Towers of Hanoi:
  Tower left;
  Tower middle;
  Tower right;
  // The selected tower, if any:
  Optional<Tower> selected;

  Hanoi() {
    this.width    = Ring.SCALE  * nRings * nTowers;
    this.height   = Ring.HEIGHT * nRings;
    this.left     = new Tower(nRings);
    this.middle   = new Tower(nRings);
    this.right    = new Tower(nRings);
    this.selected = Optional.empty();
    this.left.push(new Ring(3));
    this.left.push(new Ring(2));
    this.left.push(new Ring(1));
  }

  // Make a scene representation of the towers
  public WorldScene makeScene() {
    WorldImage towersImg =
            new BesideImage(makeTowerImage(this.left),
                    new BesideImage(makeTowerImage(this.middle),
                            makeTowerImage(this.right)));
    WorldScene scn = new WorldScene(this.width, this.height);
    scn.placeImageXY(towersImg, this.width / 2, this.height / 2);
    return scn;
  }

  // Render the given tower as an image
  // If the tower is currently selected, draw with a gray background.
  private WorldImage makeTowerImage(Tower t) {
    Color bgc = selected.isPresent() && selected.get() == t ? GRAY : WHITE;
    return t.toImage(bgc);
  }

  // Handle mouse clicks, by
  // - selecting the clicked tower, if no tower is selected;
  // - place the top ring of the selected tower on the clicked tower; or
  // - doing nothing if the clicked tower cannot accept the top ring.
  // If `this.selected = Optional.of(t)' for some tower `t', then
  // `t' is the currently selected tower.
  public void onMouseClicked(Posn event) {
    Integer towerWidth = Ring.SCALE * this.nRings;
    Tower clicked = event.x <     towerWidth ? this.left :
                    event.x < 2 * towerWidth ? this.middle :
                                               this.right;

    // Ex 3: Implement game logic here.
  }

  // Ex 1:
  // Move the top ring of `from' to `to', if possible
  private void moveRing(Tower from, Tower to) {
    return;
  }
}

class Main {

  void testBigBang(Tester t) {
    Hanoi h = new Hanoi();
    h.bigBang(h.width, h.height, 0);
  }

  IStackof<Integer> stack;

  void initData() {
    this.stack = new Stackof<>();
    this.stack.push(1);
    this.stack.push(2);
    this.stack.push(3);
  }

  void testPushPop(Tester t) {
    this.initData();
    Integer n = this.stack.height();
    this.stack.push(0);
    t.checkExpect(this.stack.height(), n+1);
    t.checkExpect(this.stack.pop().get(), 0);
    t.checkExpect(this.stack.height(), n);
    this.stack.push(0);
    this.stack.push(42);
    t.checkExpect(this.stack.height(), n+2);
    t.checkExpect(this.stack.pop().get(), 42);
    this.stack.pop();
    this.stack.pop();
    this.stack.pop();
    this.stack.pop();
    t.checkExpect(this.stack.pop().isPresent(), false);
  }

  IStackof<Integer> oStack;

  void initOrdData() {
    this.oStack = new OrdStackof<>();
    this.oStack.push(3);
    this.oStack.push(2);
    this.oStack.push(1);
  }

  void testOrdPushPop(Tester t) {
    this.initOrdData();
    Integer n = this.oStack.height();
    this.oStack.push(0);
    t.checkExpect(this.oStack.height(), n+1);
    t.checkExpect(this.oStack.pop().get(), 0);
    t.checkExpect(this.oStack.height(), n);
    this.oStack.push(4);
    t.checkExpect(this.oStack.height(), n); // 4 was not pushed on
    this.oStack.push(-4);
    t.checkExpect(this.oStack.height(), n+1); // -4 was pushed on
    t.checkExpect(this.oStack.pop().get(), -4);
    this.oStack.pop();
    this.oStack.pop();
    this.oStack.pop();
    t.checkExpect(this.oStack.pop().isPresent(), false);
  }

}
