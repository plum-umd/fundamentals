import java.awt.Color;
import java.util.Random;

import tester.*;

import funworld.*;
import geometry.*;
import colors.*;

/**
 * Copyright 2012 Viera K. Proulx
 * This program is distributed under the terms of the 
 * GNU Lesser General Public License (LGPL)
 */

/** Class that represents a colored disk that moves around the Canvas */
class Blob{
	
	Posn center;
	int radius;
	IColor col;
	//ImageMaker image = new ImageMaker("shark.png");
	
	/** The constructor */
	Blob(Posn center, int radius, IColor col) {
		this.center = center;
		this.radius = radius;
		this.col = col;
	}
	
	/** draw this blob in the given World */
	boolean draw(Canvas c) {
		//return c.drawImage(image, this.center);
		return c.drawDisk(this.center, this.radius, this.col);
	} 
	
	/** produce the image of this blob at its current location and color */
	WorldImage blobImage(){
		//return new DiskImage(this.center, this.radius, this.col);
		return new FromFileImage(this.center, "shark.png");
	}
	
	/** move this blob 20 pixels in the direction given by the ke
	 or change its color to Green, Red or Yellow */
	public Blob moveBlob(String ke){
		if (ke.equals("right")){
			return new Blob(new Posn(this.center.x + 5, this.center.y),
							this.radius, this.col);
		}
		else if (ke.equals("left")){
			return new Blob(new Posn(this.center.x - 5, this.center.y),
							this.radius, this.col);
		}
		else if (ke.equals("up")){
			return new Blob(new Posn(this.center.x, this.center.y - 5),
							this.radius, this.col);
		}
		else if (ke.equals("down")){
			return new Blob(new Posn(this.center.x, this.center.y + 5),
							this.radius, this.col);
		}
		// change the color to Y, G, R
		else if (ke.equals("Y")){
			return new Blob(this.center, this.radius, new Yellow());
		}    
		else if (ke.equals("G")){
			return new Blob(this.center, this.radius, new Green());
		}    
		else if (ke.equals("R")){
			return new Blob(this.center, this.radius, new Red());
		}
		else
			return this;
	}
	
	/** produce a new blob moved by a random distance < n pixels */
	Blob randomMove(int n){
		return new Blob(new Posn(this.center.x + this.randomInt(n),
								 this.center.y + this.randomInt(n)),
						this.radius, this.col);
	}
	
	/** helper method to generate a random number in the range -n to n */
	int randomInt(int n){
		return -n + (new Random().nextInt(2 * n + 1));
	}
	
	/** is the blob outside the bounds given by the width and height */
	boolean outsideBounds(int width, int height) {
		return this.center.x < 0
		|| this.center.x > width
		|| this.center.y < 0 
		|| this.center.y > height;
	}
	
	/** is the blob near the center of area given by the width and height */
	boolean nearCenter(int width, int height) {
		return this.center.x > width / 2 - 10
		&& this.center.x < width / 2 + 10
		&& this.center.y > height / 2 - 10
		&& this.center.y < height / 2 + 10;
	}
}

/** Represent the world of a Blob */
class BlobWorldFun extends World {
	
	int width = 200;
	int height = 300;
	Blob blob;
	
	/** The constructor */
	public BlobWorldFun(Blob blob) {
		super();
		this.blob = blob;
	}
	
	/** Move the Blob when the player presses a key */
	public World onKeyEvent(String ke) {
		return new BlobWorldFun(this.blob.moveBlob(ke));
	}
	
	/** On tick check whether the Blob is out of bounds,
	 * or fell into the black hole in the middle.
	 * If all is well, move the Blob in a random direction.
	 */
	public World onTick() {
		/*
		 // if the blob is outside the canvas, stop
		 if (this.blob.outsideBounds(this.width, this.height)){
		 return this.endOfWorld("Blob is outside the bounds");
		 }
		 
		 // time ends is the blob falls into the black hole in the middle
		 if (this.blob.nearCenter(this.width, this.height) &&
		 this.endOfTime("Black hole ate the blob"))
		 return this;
		 
		 // else move the blob randomly at most 5 pixels in any direction
		 else
		 */
		return new BlobWorldFun(this.blob.randomMove(5));
	}
	
	/**
	 * On mouse click move the blob to the mouse location, make the color red.
	 */
	public World onMouseClicked(Posn loc){
		System.out.println("Mouse click at : (" + loc.x + ", " + loc.y + ")");
		return new BlobWorldFun(new Blob(loc, 20, new Red()));
	}
	/**
	 * The entire background image for this world
	 * It illustrates the use of most of the <code>WorldImage</code> shapes
	 */
	public WorldImage blackHole = 
    new OverlayImages(new RectangleImage(new Posn(100, 150), 
										 this.width, this.height, new Blue()),
					  new OverlayImages(new EllipseImage(new Posn(12, 12), 25, 25, new Green()),
										new OverlayImages(new DiskImage(new Posn(100, 150), 10, new Black()),
														  new OverlayImages(new CircleImage(new Posn(100, 150), 10, new White()),
																			new OverlayImages(new RectangleImage(new Posn(100, 150), 10, 10, new White()),
																							  new OverlayImages(new LineImage(new Posn(95, 145), 
																															  new Posn(105, 155), new Red()),
																												new OverlayImages(
																																  new LineImage(new Posn(95, 155), new Posn(105, 145), new Red()),
																																  new OvalImage(new Posn(187,287), 25, 25, new Green())
																																  )))))));
	
	/**
	 * produce the image of this world by adding the moving blob 
	 * to the background image
	 */
	public WorldImage makeImage(){
		return new OverlayImages(blackHole, this.blob.blobImage()); 
	}
	
	/**
	 * Check whether the Blob is out of bounds,
	 * or fell into the black hole in the middle.
	 */
	public WorldEnd worldEnds (){
		// if the blob is outside the canvas, stop
		if (this.blob.outsideBounds(this.width, this.height))
			return 
			new WorldEnd(true, 
						 this.makeLastImage("Blob is outside the bounds", 
											new Posn(100, 40), Color.red));
		
		// time ends is the blob falls into the black hole in the middle
		if (this.blob.nearCenter(this.width, this.height))
			return 
			new WorldEnd(true, 
						 this.makeLastImage("Black hole ate the blob", 
											new Posn(100, 40), Color.red));
		else
			return new WorldEnd(false, this.makeImage());
	}
	
}

class BlobExamples{
	
	// examples of data for the Blob class:
	Blob b1 = new Blob(new Posn(100, 100), 50, new Red());
	Blob b1left = new Blob(new Posn(95, 100), 50, new Red());
	Blob b1right = new Blob(new Posn(105, 100), 50, new Red());
	Blob b1up = new Blob(new Posn(100, 95), 50, new Red());
	Blob b1down = new Blob(new Posn(100, 105), 50, new Red());
	Blob b1G = new Blob(new Posn(100, 100), 50, new Green());
	Blob b1Y = new Blob(new Posn(100, 100), 50, new Yellow());
	
	
	// examples of data for the BlobWorldFun class:
	BlobWorldFun b1w = new BlobWorldFun(this.b1);
	BlobWorldFun b1leftw = new BlobWorldFun(this.b1left);
	BlobWorldFun b1rightw = new BlobWorldFun(this.b1right);
	BlobWorldFun b1upw = new BlobWorldFun(this.b1up);
	BlobWorldFun b1downw = new BlobWorldFun(this.b1down);
	BlobWorldFun b1Gw = new BlobWorldFun(this.b1G);
	BlobWorldFun b1Yw = new BlobWorldFun(this.b1Y);
	BlobWorldFun b1mouse50x50w = 
    new BlobWorldFun(new Blob(new Posn(50, 50), 20, new Red()));
	
	/** test the method moveBlob in the Blob class */
	boolean testMoveBlob(Tester t){
		return
		t.checkExpect(this.b1.moveBlob("left"), 
					  this.b1left, "test moveBolb - left " + "\n") &&
		t.checkExpect(this.b1.moveBlob("right"), 
					  this.b1right, "test movelob - right " + "\n") &&
		t.checkExpect(this.b1.moveBlob("up"), 
					  this.b1up, "test moveBlob - up " + "\n") &&
		t.checkExpect(this.b1.moveBlob("down"), 
					  this.b1down, "test moveBlob - down " + "\n") &&
		t.checkExpect(this.b1.moveBlob("G"), 
					  this.b1G, "test moveBlob - G " + "\n") &&  
		t.checkExpect(this.b1.moveBlob("Y"), 
					  this.b1Y, "test moveBlob - Y " + "\n") && 
		t.checkExpect(this.b1G.moveBlob("R"), 
					  this.b1, "test moveBlob - R " + "\n");  
	}
	
	/** test the method onKeyEvent in the BlobWorldFun class */
	boolean testOnKeyEvent(Tester t){
		return
		t.checkExpect(this.b1w.onKeyEvent("left"), 
					  this.b1leftw, "test moveBolb - left " + "\n") &&
		t.checkExpect(this.b1w.onKeyEvent("right"), 
					  this.b1rightw, "test movelob - right " + "\n") &&
		t.checkExpect(this.b1w.onKeyEvent("up"), 
					  this.b1upw, "test moveBlob - up " + "\n") &&
		t.checkExpect(this.b1w.onKeyEvent("down"), 
					  this.b1downw, "test moveBlob - down " + "\n") &&
		t.checkExpect(this.b1w.onKeyEvent("G"), 
					  this.b1Gw, "test moveBlob - G " + "\n") &&  
		t.checkExpect(this.b1w.onKeyEvent("Y"), 
					  this.b1Yw, "test moveBlob - Y " + "\n") && 
		t.checkExpect(this.b1Gw.onKeyEvent("R"), 
					  this.b1w, "test moveBlob - R " + "\n");  
	}
	
	/** test the method outsideBounds in the Blob class */
	boolean testOutsideBounds(Tester t){
		return
		t.checkExpect(this.b1.outsideBounds(60, 200), true,
					  "test outsideBounds on the right") &&
		
		t.checkExpect(this.b1.outsideBounds(100, 90), true,
					  "test outsideBounds below") &&
		
		t.checkExpect(
					  new Blob(new Posn(-5, 100), 50, new Red()).outsideBounds(100, 110), 
					  true,
					  "test outsideBounds above") &&
		
		t.checkExpect(
					  new Blob(new Posn(80, -5), 50, new Blue()).outsideBounds(100, 90), 
					  true,
					  "test outsideBounds on the left") &&
		
		t.checkExpect(this.b1.outsideBounds(200, 400), false,
					  "test outsideBounds - within bounds");
	}
	
	/** test the method onMOuseClicked in the BlobWorldFun class */
	boolean testOnMouseClicked(Tester t){
		return
		t.checkExpect(this.b1w.onMouseClicked(new Posn(50, 50)), 
					  this.b1mouse50x50w);
	}
	
	/** test the method nearCenter in the Blob class */
	boolean testNearCenter(Tester t){
		return
		t.checkExpect(this.b1.nearCenter(200, 200), true,
					  "test nearCenter - true") &&
		t.checkExpect(this.b1.nearCenter(200, 100), false,
					  "test nearCenter - false");
	}
	
	/** the method randomInt in the Blob class */
	boolean testRandomInt(Tester t){
		return
		t.checkOneOf("test randomInt",
					 this.b1.randomInt(3), -3, -2, -1, 0, 1, 2, 3) &&
		t.checkNoneOf("test randomInt", 
					  this.b1.randomInt(3), -5, -4, 4, 5);
	}
	
	/** test the method randomMove in the Blob class */
	boolean testRandomMove(Tester t){
		return 
		t.checkOneOf("test randomMove", this.b1.randomMove(1),
					 new Blob(new Posn( 99,  99), 50, new Red()),
					 new Blob(new Posn( 99, 100), 50, new Red()),
					 new Blob(new Posn( 99, 101), 50, new Red()),
					 new Blob(new Posn(100,  99), 50, new Red()),
					 new Blob(new Posn(100, 100), 50, new Red()),
					 new Blob(new Posn(100, 101), 50, new Red()),
					 new Blob(new Posn(101,  99), 50, new Red()),
					 new Blob(new Posn(101, 100), 50, new Red()),
					 new Blob(new Posn(101, 101), 50, new Red()));
	}  
	
	/** test the method onTick in the BlobWorldFun class */
	boolean testOnTick1(Tester t){
		boolean result = true;
		for (int i = 0; i < 20; i++){
			BlobWorldFun bwf = (BlobWorldFun)this.b1w.onTick();
			result = result &&
			t.checkRange(bwf.blob.center.x, 95, 106) &&
			t.checkRange(bwf.blob.center.y, 95, 106);
		}
		return result;
	}
	
	/** test the method onTick in the BlobWorldFun class */
	boolean testOnTick2(Tester t){
		return
		
		// insufficient number of options ...
		t.checkOneOf("test onTick2: randomMove", this.b1w.onTick(),
					 new BlobWorldFun(new Blob(new Posn( 99,  99), 50, new Red())),
					 new BlobWorldFun(new Blob(new Posn( 99, 100), 50, new Red())),
					 new BlobWorldFun(new Blob(new Posn( 99, 101), 50, new Red())),
					 new BlobWorldFun(new Blob(new Posn(100,  99), 50, new Red())),
					 new BlobWorldFun(new Blob(new Posn(100, 100), 50, new Red())),
					 new BlobWorldFun(new Blob(new Posn(100, 101), 50, new Red())),
					 new BlobWorldFun(new Blob(new Posn(101,  99), 50, new Red())),
					 new BlobWorldFun(new Blob(new Posn(101, 100), 50, new Red())),
					 new BlobWorldFun(new Blob(new Posn(101, 101), 50, new Red()))
					 ); 
	}
	
	// test the method worldEnds for the class BlobWorld
	// ........ to be done ..............
	boolean testWorldEnds(Tester t){
		return true;
	}
	
	/** run the animation */
	BlobWorldFun w1 = 
	new BlobWorldFun(new Blob(new Posn(100, 200), 20, new Red()));
	BlobWorldFun w2 = 
    new BlobWorldFun(new Blob(new Posn(100, 200), 20, new Red()));
	BlobWorldFun w3 = 
    new BlobWorldFun(new Blob(new Posn(100, 200), 20, new Red()));
	
	// test that we can run three different animations concurrently
	// with the events directed to the correct version of the world
	
	boolean runAnimation = this.w1.bigBang(200, 300, 0.3); 
	boolean runAnimation2 = this.w2.bigBang(200, 300, 0.3); 
	boolean runAnimation3 = this.w3.bigBang(200, 300, 0.3); 
	
	
	/** main: an alternative way of starting the world and running the tests */
	public static void main(String[] argv){
		BlobWorldFun w = 
        new BlobWorldFun(new Blob(new Posn(150, 100), 20, new Red()));
		w.bigBang(200, 300, 0.3);
		BlobExamples be = new BlobExamples();
		Tester.run(be);
		/* 
		 Canvas c = new Canvas(200, 300);
		 c.show();
		 System.out.println(" let's see: \n\n" + 
		 Printer.produceString(w.makeImage()));
		 c.drawImage(new OverlayImages(new DiskImage(new Posn(50, 50), 20, new Red()),
		 new RectangleImage(new Posn(20, 30), 40, 20, new Blue())));
		 */
	}
	
}