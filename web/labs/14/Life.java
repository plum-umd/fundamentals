//
// Conway's Game of Life
// Fundies 2 Honors
//

import tester.*;
import funworld.*;
import geometry.*;
import java.awt.Color;
import java.util.*;

// A Cell is a
//   new Cell()
//
// and implements
//
// setLive : -> void
// Sets cell to live
// effect: updates live/dead
//
// setDead : -> void
// Sets cell to dead
// effect: updates live/dead
//
// isLive : -> Boolean
// Checks if cell is live
//
// makeImage : -> WorldImage
// Produces an image for this cell
//
// tick : -> void
// Runs the rules of the game
// effect: updates live/dead
//
// addNeighbor : Cell -> void
// Adds a new neighbor cell
// effect: updates neighbors
//
// updateLiveness : -> void
// Sets the liveness from neighbors
// effect: updates liveness

class Cell {
  Boolean isLive;
  List<Cell> neighbors;
  Integer liveness;

  public Cell() {
    this.isLive = false;
    this.neighbors = new ArrayList<Cell>();
    this.liveness = 0;
  }

  public void setLive() {
    this.isLive = true;
  }

  public void setDead() {
    this.isLive = false;
  }

  public Boolean isLive() {
    return this.isLive;
  }

  public WorldImage makeImage() {
    java.awt.Color color;
    if (isLive())
      color = java.awt.Color.black;
    else
      color = java.awt.Color.white;

    WorldImage shape = new RectangleImage(new Posn(10, 10), 20, 20, color);

    return shape.overlayImages(new LineImage(new Posn(0, 20), new Posn(0, 0), java.awt.Color.black),
                               new LineImage(new Posn(20, 20), new Posn(20, 0), java.awt.Color.black),
                               new LineImage(new Posn(0, 20), new Posn(20, 20), java.awt.Color.black),
                               new LineImage(new Posn(0, 0), new Posn(20, 0), java.awt.Color.black));
  }

  public void addNeighbor(Cell c) {
    neighbors.add(c);
  }

  public void tick() {
    if (this.isLive) {
      if (liveness.compareTo(2) < 0)
        this.isLive = false;
      else if (liveness.equals(2) || liveness.equals(3))
        this.isLive = true;
      else
        this.isLive = false;
    }
    else {
      if (liveness.equals(3))
        this.isLive = true;
      else
        this.isLive = false;
    }
  }

  public void updateLiveness() {
    this.liveness = 0;
    for (Cell c : neighbors) {
      if (c.isLive())
        liveness += 1;
    }
  }
}

// The World keeps track of the board which
// is a list of lists.

class MyWorld extends World {
  // some constants
  public Integer cellSize = 20;
  public Integer m = 25;

  ArrayList<ArrayList<Cell>> board;
  Boolean paused;

  MyWorld() {
    paused = false;
    board = new ArrayList<ArrayList<Cell>>();

    // A for loop instead of an iterator
    // because we count up to a constant
    for (int i = 0; i < m; i++) {
      ArrayList<Cell> row = new ArrayList<Cell>();
      for (int j = 0; j < m; j++) {
        row.add(new Cell());
      }
      board.add(row);
    }

    // Here we can use list iterators
    ListIterator<ArrayList<Cell>> bi = board.listIterator();
    while (bi.hasNext()) {
      Integer i = bi.nextIndex();
      ListIterator<Cell> ri = bi.next().listIterator();
      while (ri.hasNext()) {
        Integer j = ri.nextIndex();
        Cell current = ri.next();
        addNeighborAt(current, i, j-1);
        addNeighborAt(current, i, j+1);
        addNeighborAt(current, i-1, j);
        addNeighborAt(current, i-1, j-1);
        addNeighborAt(current, i-1, j+1);
        addNeighborAt(current, i+1, j);
        addNeighborAt(current, i+1, j-1);
        addNeighborAt(current, i+1, j+1);
      }
    }
  }

  private void addNeighborAt(Cell c, int i, int j) {
    // Check that it's within bounds
    if (i >= 0 && i < board.size() && j >= 0 && j < board.size()) {
        c.addNeighbor(board.get(i).get(j));
    }
  }

  public WorldImage makeImage() {
    WorldImage boardImg = new RectangleImage(new Posn(0, 0), 0, 0, java.awt.Color.white);

    for (ArrayList<Cell> row : board) {
      WorldImage rowImg = new RectangleImage(new Posn(0, 0), 0, 0, java.awt.Color.white);
      for (Cell cell : row) {
        rowImg.movePinhole(20, 0);
        rowImg = new OverlayImagesXY(rowImg, cell.makeImage(), 0, 0);
      }
      boardImg.movePinhole(0, 20);
      boardImg = new OverlayImagesXY(boardImg, rowImg, 0, 0);
    }

    return boardImg;
  }

  public World onTick() {
    if (!paused) {
      // first update liveness
      for (ArrayList<Cell> row : board) {
        for (Cell c : row) {
          c.updateLiveness();
        }
      }
      // then tick each cell
      // the ordering is crucial
      for (ArrayList<Cell> row : board) {
        for (Cell c : row) {
          c.tick();
        }
      }
    }

    return this;
  }

  public World onKeyEvent(String key) {
    // spacebar to pause
    if (key.equals(" "))
      paused = !paused;

    return this;
  }

  public World onMouseClicked(Posn mousePos) {
    Integer xi = Math.round(mousePos.x / (Driver.canvasWidth / board.size()));
    Integer yi = Math.round(mousePos.y / (Driver.canvasHeight / board.size()));

    board.get(board.size() - yi - 1).get(board.size() - xi - 1).setLive();

    return this;
  }
}

class Driver {
  // Since you can't find the size of the canvas any other way,
  // we'll assume some constants
  static public Integer canvasWidth = 501;
  static public Integer canvasHeight = 501;

  public static void main(String[] args) {
    (new MyWorld()).bigBang(canvasWidth, canvasHeight, 0.1);
  }
}
