import java.awt.Color;
import java.util.Random;

import tester.*;

import funworld.*;
import geometry.*;
import colors.*;;

/**
 * Copyright 2011 Viera K. Proulx
 * This program is distributed under the terms of the 
 * GNU Lesser General Public License (LGPL)
 */

class House{
  Posn loc;    // of the SW corner of the house base
  int width;   // the width of the house
  int height;  // the height of the house
  Color color; // the color of the house
  
  House(Posn loc, int width, int height, Color color){
    this.loc = loc;
    this.width = width;
    this.height = height;
    this.color = color;
  }
  
  // make the image of this house
  WorldImage houseImage(){
    return 
    new RectangleImage(
        // the pinhole in the center
        new Posn(this.loc.x + this.width / 2, this.loc.y - this.height / 2), 
        this.width, this.height, this.color).
    overlayImages(
        // the roof - the height is half the height of the house
        new TriangleImage(new Posn(this.loc.x, this.loc.y - this.height), 
                          new Posn(this.loc.x + this.width, this.loc.y - this.height),
                          new Posn(this.loc.x + this.width / 2, 
                                   this.loc.y - 3 * this.height / 2),
                          Color.red),
        // the door - black in the middle
        new RectangleImage(new Posn(this.loc.x + this.width / 2,
                                    this.loc.y - this.height / 4),
                           this.width / 2, this.height / 2, Color.gray));
  }
}

class Tree{
  Posn loc;         // the left base of the trunk
  int trunkHeight;  // the height of the trunk
  int width;        // the width of the crown of the tree
  int height;       // the height of the crown of the tree
  
  Tree(Posn loc, int trunkHeight, int width, int height){
    this.loc = loc;
    this.trunkHeight = trunkHeight;
    this.width = width;
    this.height = height;
  }
  
  WorldImage treeImage(){
    return
    // the trunk
    new RectangleImage(new Posn(this.loc.x + 5, 
                                this.loc.y - this.trunkHeight / 2),
                       10, this.trunkHeight, 
                       new Color(0x84, 0x3c, 0x24)).overlayImages(
    new OvalImage(new Posn(this.loc.x + 5, 
                           this.loc.y - this.trunkHeight - this.height / 2 + 3),
                  this.width, this.height, Color.green));
  }
}

class Cloud{
  Posn loc;
  int width;
  int height;
  
  Cloud(Posn loc, int width, int height){
    this.loc = loc;
    this.width = width;
    this.height = height;
  }
  
  Cloud move(int dx){
    return new Cloud(new Posn(this.loc.x + dx, this.loc.y), 
        this.width, this.height);
  }

  Cloud moveInBounds(int dx, int vbound){
    if (this.loc.x + dx > vbound)
      return new Cloud(new Posn(dx, this.loc.y), 
          this.width, this.height);
    else
      return this.move(dx);
  }
  
  WorldImage cloudImage(){
    return
    new OvalImage(this.loc, this.width, this.height, Color.white).overlayImages(
    new OvalImage(new Posn(this.loc.x - this.width / 4, 
                      this.loc.y - this.height / 4),
                  this.width / 2, this.width / 2, Color.white),
    new OvalImage(new Posn(this.loc.x + this.width / 4, 
                      this.loc.y - this.height / 2),
                  this.width / 2, this.height / 2, Color.white));       
  }
}

class Sun{
  int size;
  
  Sun(int size){
    this.size = size % 30;
  }
  
  WorldImage sunImage(){
    return 
    new DiskImage(new Posn(50, 50), this.size, new Color(255, 255, 0, 230));
  }
}

/** Class that represents little houses with clouds above */
class TickyTack extends World{
  House h1 = new House(new Posn(0, 300), 60, 80, Color.red);
  House h2 = new House(new Posn(60, 300), 60, 40, Color.green);
  House h3 = new House(new Posn(120, 300), 80, 60, Color.pink);
  House h4 = new House(new Posn(200, 300), 70, 50, Color.cyan);
  House h5 = new House(new Posn(270, 300), 90, 70, Color.yellow);
  House h6 = new House(new Posn(360, 300), 80, 60, Color.magenta);
  House h7 = new House(new Posn(440, 300), 90, 70, Color.orange);
  
  Tree t1 = new Tree(new Posn(550, 300), 40, 50, 80);
  Tree t2 = new Tree(new Posn(580, 300), 20, 30, 30);
  
  Cloud cloud;
  
  Sun sun = new Sun(20);
  
  TickyTack(Cloud cloud, Sun sun){
    this.cloud = cloud;
    this.sun = sun;
  }
  
  public World onTick(){
    return new TickyTack(this.cloud.moveInBounds(4, 600), this.sun);
  }

  public World onKeyEvent(String ke){
    if (ke.equals(" "))
      return new TickyTack(this.cloud, new Sun(this.sun.size + 3));
    else
      return this;
  }
  
  public WorldImage makeImage(){
    return 
    new RectangleImage(new Posn(300, 150), 600, 300, Color.blue).overlayImages(
        this.h1.houseImage(),
        this.h2.houseImage(),
        this.h3.houseImage(),
        this.h4.houseImage(),
        this.h5.houseImage(),
        this.h6.houseImage(),
        this.h7.houseImage(),
        this.t1.treeImage(),
        this.t2.treeImage(),
        this.cloud.cloudImage(),
        this.sun.sunImage());
  }
  
}

class ExamplesTickyTack{
  ExamplesTickyTack(){}
  
  House h1 = new House(new Posn(0, 300), 60, 80, Color.red);
  House h2 = new House(new Posn(60, 300), 60, 40, Color.green);
  House h3 = new House(new Posn(120, 300), 80, 60, Color.pink);
  House h4 = new House(new Posn(200, 300), 70, 50, Color.cyan);
  House h5 = new House(new Posn(270, 300), 90, 70, Color.yellow);
  House h6 = new House(new Posn(360, 300), 80, 60, Color.magenta);
  House h7 = new House(new Posn(440, 300), 90, 70, Color.orange);
  
  Tree t1 = new Tree(new Posn(550, 300), 40, 50, 80);
  Tree t2 = new Tree(new Posn(580, 300), 20, 30, 30);
  
  Cloud cloud = new Cloud(new Posn(200, 100), 90, 60);
  
  Sun sun = new Sun(25);
  
  WorldImage wholeworld = 
      new RectangleImage(new Posn(300, 150), 600, 300, Color.blue).overlayImages(
          this.h1.houseImage(),
          this.h2.houseImage(),
          this.h3.houseImage(),
          this.h4.houseImage(),
          this.h5.houseImage(),
          this.h6.houseImage(),
          this.h7.houseImage(),
          this.t1.treeImage(),
          this.t2.treeImage(),
          this.cloud.cloudImage(),
          this.sun.sunImage());
  
  Canvas c = new Canvas(600, 300);
  
  boolean showWorld = 
      c.show() && c.drawImage(this.wholeworld);

  TickyTack tworld = new TickyTack(this.cloud, this.sun);
  
  boolean testWholeWorld(Tester t){
    return this.tworld.bigBang(600, 300, 0.1);
  }
  
  boolean testOnTick(Tester t){
    return true;
  }
  
  public static void main(String[] argv){
    ExamplesTickyTack ett = new ExamplesTickyTack();
    
    
    TickyTack tworld = new TickyTack(ett.cloud, ett.sun);
    tworld.bigBang(600, 300, 0.1);
  }
  
}