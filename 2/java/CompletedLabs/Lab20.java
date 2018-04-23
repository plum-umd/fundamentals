package edu.umd.cmsc132A;

import javalib.impworld.World;
import javalib.impworld.WorldScene;
import javalib.worldimages.*;
import tester.Tester;

import java.awt.*;
import java.util.Optional;

import static java.awt.Color.GRAY;
import static java.awt.Color.WHITE;
import static javalib.worldimages.AlignModeX.CENTER;
import static javalib.worldimages.AlignModeY.BOTTOM;
import static javalib.worldimages.OutlineMode.SOLID;

public class Lab20 { /* Intentionally blank; leave blank */ }

class Ring implements Comparable<Ring> {

  static Integer SCALE = 50;
  static Integer HEIGHT = 25;
  Integer width;

  Ring(Integer width) {
    this.width = width;
  }

  public int compareTo(Ring o) {
    return this.width.compareTo(o.width);
  }

  // Render this ring as an image
  public WorldImage toImage() {
    return new RectangleImage(this.width * Ring.SCALE, Ring.HEIGHT, SOLID, Color.RED);
  }

}

class Tower extends OrdStackof<Ring> {
  Integer maxRings;

  Tower(Integer maxRings) {
    this.maxRings = maxRings;
  }

  // Render this tower as an image
  public WorldImage toImage(Color bgc) {
    Integer postWidth = Ring.SCALE / 3;
    Integer width  = this.maxRings * Ring.SCALE;
    Integer height = this.maxRings * Ring.HEIGHT;
    WorldImage b = new EmptyImage();
    WorldImage rings = this.foldr((r, i) ->
            new AboveAlignImage(CENTER, r.toImage(), i), b);
    WorldImage post = new RectangleImage(postWidth, height, SOLID, Color.BLACK);
    WorldImage bg = new RectangleImage(width, height, SOLID, bgc);
    return new OverlayOffsetAlign(CENTER, BOTTOM, rings, 0, 0, post)
            .overlayImages(bg);
  }
}

class Hanoi3 extends World {

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

  Hanoi3() {
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
  public void onMouseClicked(Posn event) {
    Integer towerWidth = Ring.SCALE * this.nRings;
    Tower clicked = event.x <     towerWidth ? this.left :
                    event.x < 2 * towerWidth ? this.middle :
                                               this.right;
    if (this.selected.isPresent()) {
      Tower from = this.selected.get();
      if (from != clicked) {
        this.moveRing(from, clicked);
      }
      this.selected = Optional.empty();
    } else if (clicked.height() > 0) {
      this.selected = Optional.of(clicked);
    }
  }

  // Move the top ring of `from' to `to', if possible.
  // Invariant: `from' has at least one ring
  private void moveRing(Tower from, Tower to) {
    Ring top = from.pop().get();
    if (to.canPush(top)) {
      to.push(top);
    } else {
      from.push(top);
    }
  }
}

class HanoiN extends World {

  Integer nRings;            // the number of rings  in this world
  Integer nTowers;           // the number of towers in this world
  Integer width;             // the pixel width  of the scene
  Integer height;            // the pixel height of the scene
  Listof<Tower> towers;      // the towers of Hanoi
  Optional<Tower> selected;  // the selected tower, if any

  HanoiN(Integer nRings, Integer nTowers) {
    if (nRings < 0 || nTowers < 0) {
      throw new RuntimeException("There must be a positive number of rings and towers!");
    }
    this.nRings   = nRings;
    this.nTowers  = nTowers;
    this.width    = Ring.SCALE  * nRings * nTowers;
    this.height   = Ring.HEIGHT * nRings;
    this.towers   = AListof.make(nTowers, n -> new Tower(nRings));
    this.selected = Optional.empty();
    // Add rings to leftmost tower:
    Tower leftmost = this.towers.first().get();
    AListof.make(nRings, n -> new Ring(nRings - n))
            .forEach(r -> leftmost.push(r));
  }

  // Make a scene representation of the towers
  public WorldScene makeScene() {
    WorldImage towersImg = this.towers.foldr((Tower tower, WorldImage img) ->
                    new BesideImage(this.makeTowerImage(tower), img),
            new EmptyImage());
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
  public void onMouseClicked(Posn event) {
    Integer towerWidth = Ring.SCALE * this.nRings;
    Integer towerClicked = Math.min(event.x / towerWidth, this.nTowers);
    Tower clicked = this.towers.nth(towerClicked).get();
    if (this.selected.isPresent()) {
      Tower from = this.selected.get();
      if (from != clicked) {
        this.moveRing(from, clicked);
      }
      this.selected = Optional.empty();
    } else if (clicked.height() > 0) {
      this.selected = Optional.of(clicked);
    }
  }

  // Move the top ring of `from' to `to', if possible.
  // Invariant: `from' has at least one ring
  private void moveRing(Tower from, Tower to) {
    Ring top = from.pop().get();
    if (to.canPush(top)) {
      to.push(top);
    } else {
      from.push(top);
    }
  }
}

class Main {

  void testBigBang(Tester t) {
    HanoiN h = new HanoiN(5, 3);
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
