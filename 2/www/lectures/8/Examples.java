import tester.*;
import javalib.funworld.*;
import javalib.worldimages.*;
import java.awt.Color;

public class Examples {
    public static void main(String[] args) {
	new LightWorld().bigBang(100, 100, 1);
    }
}

class LightWorld extends World {
    Light l;

    LightWorld(Light l) {
	this.l = l;
    }

    public WorldScene makeScene() {
	return new WorldScene(100, 100)
	    .placeImageXY(new CircleImage(50, OutlineMode.SOLID, Color.RED), 50, 50);
    }
}


interface Light {
    Light next();
    WorldImage draw();
}

class Red implements Light {
    public Light next() {
	return this;
    }

    public WorldImage draw() {
	return new CircleImage(50, OutlineMode.SOLID, Color.RED);
    }
}

class Example {
    public Integer five() {
	return 5;
    }
}

class ExampleTests {
    boolean testFive(Tester t) {
	return t.checkExpect(new Example().five(), 5);
    }
}