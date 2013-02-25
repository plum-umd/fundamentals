import tester.*;
import funworld.*;
import geometry.*;
import colors.*;
import java.awt.Color;

class NotClicked extends World {
    NotClicked() {}
    public World onMouseClicked(Posn mouse) {
	//....
    }

    public WorldImage makeImage() {...}
}

class Clicked extends World {
    Posn mouse;
    Clicked(Posn mouse) {
	this.mouse = mouse;
    }
    
    public World onMouseClicked(Posn mouse) {
	//....
    }

    public WorldImage makeImage() {...}
}

    

class SimpleWorld extends World {

    String key;
    Posn mouse;

    SimpleWorld() {
	this("");
    }

    SimpleWorld(String key, Posn mouse) {
	this.key = key;
	this.mouse = mouse;
    }

    public World onTick() { return this; }

    public World onMouseClicked(Posn mouse) {
	// ...
    }

    public WorldImage makeImage() {
	return 
	    new RectangleImage(new Posn(300, 150), 
			       600, 
			       300, 
			       Color.blue)
	    .overlayImages(new TextImage(new Posn(300, 150), 
					 this.key, 
					 100, 
					 Color.white));
    }

    public World onKeyEvent(String key) {
	if (key.equals("q")) {
	    this.stopWorld();
	    return this;
	} else {
	    return new SimpleWorld(key);
	}
    }

    public static void main(String[] args) {
	new SimpleWorld().bigBang(600, 300, 0.1);
    }
}
