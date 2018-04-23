package edu.umd.cmsc132A;

import javalib.worldimages.*;

import java.awt.*;

import static javalib.worldimages.OutlineMode.SOLID;
import static javalib.worldimages.AlignModeX.CENTER;
import static javalib.worldimages.AlignModeY.BOTTOM;

// This class should not need to be changed.

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
                new AboveAlignImage(AlignModeX.CENTER, r.toImage(), i), b);
        WorldImage post = new RectangleImage(postWidth, height, SOLID, Color.BLACK);
        WorldImage bg = new RectangleImage(width, height, SOLID, bgc);
        return new OverlayOffsetAlign(CENTER, BOTTOM, rings, 0, 0, post)
                .overlayImages(bg);
    }
}
