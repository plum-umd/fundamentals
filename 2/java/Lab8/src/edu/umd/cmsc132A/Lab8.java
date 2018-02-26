// Authors: partner1, partner2
// Lab 8

package edu.umd.cmsc132A;

import tester.Tester;
import javalib.worldimages.*;
import javalib.funworld.*;
import java.awt.Color;
import java.util.Random;

public class Lab8 { /* Intentionally blank; leave blank */ }

// the squares of the Tic Tac Toe board
interface ISquare {
  WorldImage draw();  // draw the Tic Tac Toe square
  Boolean isMarked(); // is the square blank or has it been marked?
}

abstract class Square implements ISquare {
  static Integer SCALE = 100;

  Square() {}

  public WorldImage draw() {
    return new RectangleImage(Square.SCALE,
                              Square.SCALE,
                              OutlineMode.OUTLINE,
                              Color.BLACK);
  }

  public Boolean isMarked() {
    return false;
  }
  
}

class Blank extends Square {}

class X extends Square {
  public WorldImage draw() {
    WorldImage background = super.draw();
    WorldImage line1 =
      new LineImage(new Posn(Square.SCALE, Square.SCALE), Color.BLACK);
    WorldImage line2 =
      new LineImage(new Posn(Square.SCALE, 0 - Square.SCALE), Color.BLACK);
    return new OverlayImage(line2, new OverlayImage(line1, background));
  }

  public Boolean isMarked() {
    return true;
  }
}

class O extends Square {
  public WorldImage draw() {
    WorldImage background =
      new RectangleImage(Square.SCALE,
                         Square.SCALE,
                         OutlineMode.OUTLINE,
                         Color.BLACK);
    WorldImage circle =
      new CircleImage(Square.SCALE / 2, OutlineMode.OUTLINE, Color.BLACK);
    return new OverlayImage(circle, background);
  }

  public Boolean isMarked() {
    return true;
  }
}

class Board {
  static Integer WIDTH  = Square.SCALE * 3;
  static Integer HEIGHT = Square.SCALE * 3;

  Square[][] board;

  Board() {
    Square[][] newBoard = new Square[3][3];
    for (int x = 0; x < 3; x++) {
      for (int y = 0; y < 3; y++) {
        newBoard[x][y] = new Blank();
      }
    }
    this.board = newBoard;
  }

  Board(Square[][] board) {
    this.board = board;
  }

  WorldScene draw() {
    WorldScene scene = new WorldScene(this.WIDTH, this.HEIGHT);
    for (int x = 0; x < 3; x++) {
      for (int y = 0; y < 3; y++) {
        Integer xloc = x * Square.SCALE + (Square.SCALE / 2);
        Integer yloc = y * Square.SCALE + (Square.SCALE / 2);
        scene = scene.placeImageXY(board[x][y].draw(), xloc, yloc);
      }
    }
    return scene;
  }

  Boolean canMark(Integer x, Integer y) {
    return x >= 0 && y >= 0 && x <= 2 && y <= 2 && !board[x][y].isMarked();
  }

  Board mark(Square sq, Integer x, Integer y) {
    if (canMark(x, y)) {
      Square[][] newBoard = board.clone();
      newBoard[x][y] = sq;
      return new Board(newBoard);
    } else {
      return this;
    }
  }
}


/*

XTurn -> OTurn -> XTurn -> ... -> Done

 */



interface TicTacToeTurn {
  public World mark(Integer x, Integer y);
  public Boolean canMark(Integer x, Integer y);
}

/*
class XTurn extends World {
  
}

*/

class TicTacToe extends World {
  static Random r = new Random();  // the random number generator
  static Integer WIDTH = Square.SCALE * 3;      // the width of the scene
  static Integer HEIGHT = Square.SCALE * 3;     // the height of the scene
  static Double TICKRATE = 0.0;    // the tick rate (seconds per tick)

  Board board;  // the balls of this world

  TicTacToe() {
    this(new Board());
  }

  TicTacToe(Board board) {
    this.board = board;
  }

  public World onMouseClicked(Posn mouse) {
    return this;

    //Integer clickX = mouse.x / Square.SCALE;
    //Integer clickY = mouse.y / Square.SCALE;

    
  }

  // Tick this world's balls
  public World onTick() {
    return this;
    //return new BallWorld(this.balls.next());
  }

  // Place the balls on the scene
  public WorldScene makeScene() {
    return board.draw();
  }

}

//-----------------------------------------------------------------------------
// Main

// You can ignore this, it just gets bigBang up and running
class Main {
  Boolean testTicTacToe(Tester t) {
    TicTacToe w = new TicTacToe();
    return w.bigBang(TicTacToe.WIDTH, TicTacToe.HEIGHT, TicTacToe.TICKRATE);
  }
}
