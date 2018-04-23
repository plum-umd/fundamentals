package edu.umd.cmsc132A;

import javalib.worldimages.RectangleImage;
import javalib.worldimages.WorldImage;

import java.awt.*;

import static javalib.worldimages.OutlineMode.SOLID;

// This class should not need to be changed.

class Ring implements Comparable<Ring> {

    static Integer SCALE = 50;
    static Integer HEIGHT = 25;
    Integer width;

    Ring(Integer width) {
        this.width = width;
    }

    // Compare the width of this ring to the width of the given ring
    public int compareTo(Ring o) {
        return this.width.compareTo(o.width);
    }

    // Render this ring as an image
    public WorldImage toImage() {
        Integer width = this.width * Ring.SCALE;
        return new RectangleImage(width, Ring.HEIGHT, SOLID, Color.RED);
    }

}