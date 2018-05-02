package edu.umd.cmsc132A;

import javalib.funworld.World;
import javalib.funworld.WorldScene;
import javalib.worldimages.*;

import java.awt.*;

public class TicTacToe extends World {
    static Integer size = Square.SCALE * 3;

    Grid<Square> grid;  // the squares of the game board
    Boolean xTurn;      // true if it is X's turn, false otherwise

    public TicTacToe() {
        this(new Grid<>(AListof.make(3, m ->
                        AListof.make(3, n -> new Blank()))),
                true);
    }

    private TicTacToe(Grid<Square> grid, Boolean xTurn) {
        this.grid = grid;
        this.xTurn = xTurn;
    }

    public Boolean gameOver() {
        Listof<Listof<Square>> board = grid.unzip();
        // Ex
        return false;
    }

    public WorldScene lastScene(String s) {
        WorldImage msg = new TextImage(s, Square.FONTSIZE, Color.RED);
        return makeScene().placeImageXY(msg, size/2, size/2);
    }

    public World mark() {
        Square focus = grid.focus();
        Square marked = focus.accept(new SquareVisitor<Square>() {
            public Square visitBlank(Blank b) {
                return xTurn ? new X() : new O();
            }
            public Square visitX(X x) {
                return x;
            }
            public Square visitO(O o) {
                return o;
            }
        });
        if (this.gameOver()) {
            return this.endOfWorld(xTurn ? "X Wins!" : "O Wins!");
        } else {
            return new TicTacToe(grid.updateFocus(sq -> marked),
                    focus == marked ? xTurn : !xTurn);
        }
    }

    public WorldScene makeScene() {
        return new WorldScene(size, size).placeImageXY(toImage(), size/2, size/2);
    }

    public World onKeyEvent(String key) {
        if (key.equals("left")) {
            return new TicTacToe(this.grid.left(), xTurn);
        } else if (key.equals("right")) {
            return new TicTacToe(this.grid.right(), xTurn);
        } else if (key.equals("up")) {
            return new TicTacToe(this.grid.up(), xTurn);
        } else if (key.equals("down")) {
            return new TicTacToe(this.grid.down(), xTurn);
        } else if (key.equals("enter")) {
            return this.mark();
        } else {
            return this;
        }
    }

    public WorldImage toImage() {
        Square focused = this.grid.focus();
        Listof<Listof<Square>> grid = this.grid.unzip();
        WorldImage mt = new EmptyImage();
        return grid.foldr(
                (row, img) -> new AboveAlignImage(
                        AlignModeX.CENTER,
                        row.foldr((sq, rowImg) -> {
                                    WorldImage sqImg = sq == focused
                                            ? sq.toImage(Color.RED)
                                            : sq.toImage();
                                    return new BesideImage(sqImg, rowImg);
                                },
                                mt),
                        img),
                mt);
    }

}

interface SquareVisitor<R> {
    R visitBlank(Blank b);
    R visitX(X x);
    R visitO(O o);
}

interface Square {
    static Integer SCALE = 50;
    static Integer FONTSIZE = 36;
    static Color FONTCOLOR = Color.BLUE;
    WorldImage toImage();
    WorldImage toImage(Color background);
    <R> R accept(SquareVisitor<R> visitor);
}

class Blank implements Square {
    public WorldImage toImage() {
        return new RectangleImage(
                Square.SCALE, Square.SCALE,
                OutlineMode.OUTLINE, Color.BLACK);
    }

    public WorldImage toImage(Color background) {
        return new RectangleImage(
                Square.SCALE, Square.SCALE,
                OutlineMode.SOLID, background);
    }

    public <R> R accept(SquareVisitor<R> visitor) {
        return visitor.visitBlank(this);
    }
}

abstract class Text extends Blank {
    abstract String text();

    public WorldImage toImage() {
        WorldImage txt = new TextImage(text(), Square.FONTSIZE, Square.FONTCOLOR);
        return txt.overlayImages(super.toImage());
    }

    public WorldImage toImage(Color background) {
        WorldImage txt = new TextImage(text(), Square.FONTSIZE, Square.FONTCOLOR);
        return txt.overlayImages(super.toImage(background));
    }
}

class X extends Text {
    public String text() { return "X"; }

    public <R> R accept(SquareVisitor<R> visitor) {
        return visitor.visitX(this);
    }
}

class O extends Text {
    public String text() { return "O"; }

    public <R> R accept(SquareVisitor<R> visitor) {
        return visitor.visitO(this);
    }
}
