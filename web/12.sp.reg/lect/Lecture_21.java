/***********************************************
 *  CS2510 Spring 2011 
 *  Lecture #21
 *  More Loops, with a little World Fun
 ***********************************************/

import tester.*;
import image.*;
import world.*;
import java.util.*;

/** Crazy Sparkling Animation */
class EyecolorWorld extends VoidWorld{
    int WIDTH = 500;
    int HEIGHT = 500;


    // A list of all the sparkles
    ArrayList<Posn> l = new ArrayList<Posn>();
    // Random number generator
    Random randy = new Random();
    
    // List of possible Colors
    ArrayList<String> colors;

    /** Construct and EyeColorWorld. Create the list of possible Colors */
    EyecolorWorld(){
        this.colors = new ArrayList<String>();
        this.colors.add("indigo");
        this.colors.add("chartreuse");
        this.colors.add("red");
        this.colors.add("goldenrod");
        this.colors.add("blue");
        this.colors.add("turquoise");
        this.colors.add("green");
        this.colors.add("purple");
    }
    
    /** Draw random stars at the Posn locations */
    public Scene onDraw(){
        return this.drawAll();
    }
    
    /** Move all the sparkles and remove any that drifted offscreen */
    public void onTick(){
        this.l = this.moveAll(this.l);
        this.l = this.rmOff();
    }
    
    /** Get the color for the given index */
    String color(int i){
        return this.colors.get(i);
    }
    
    /** Produce a new list with all the Posns randomly moved */
    ArrayList<Posn> moveAll(ArrayList<Posn> l){
        ArrayList<Posn> acc = new ArrayList<Posn>();
        Iterator<Posn> it = l.iterator();
        
        while(it.hasNext()){
            Posn p = it.next();
            acc.add(new Posn(p.x + this.randy.nextInt(21)-10,
                             // Modified from the original to slide them
                             //   down off screen so we don't get stuck
                             p.y + this.randy.nextInt(21)));
        }
        return acc;
    }

    /** Draw all the Posns */
    Scene drawAll(){
        Scene acc = new EmptyScene(this.WIDTH, this.HEIGHT);
        for(int i = 0; i < this.l.size(); i = i+1){
            acc = acc.placeImage(new Star(this.randy.nextInt(10)+15,
                                          this.randy.nextInt(5)+3,"solid",
                                          this.color(this.randy.nextInt(this.colors.size()))),
                                 this.l.get(i));
        }
        return acc;
    }

    /** Add a Posn when the mouse is dragged */
    public void onMouse(int x, int y, String me){
        try{
            if(me.equals("drag")){
                this.l.add(new Posn(x, y));
            }
        }catch(ConcurrentModificationException e){
            // If the exception is thrown we can just ignore
            //   it, and not do anything
        }
    }
        
    /** Remove off screen Posns */
    ArrayList<Posn> rmOff(){
        ArrayList<Posn> acc = new ArrayList<Posn>();
        
        for(Posn p : this.l){
            if(p.y < this.HEIGHT+10 && p.y > -10 &&
               p.x < this.WIDTH+10 && p.x > -10){
                acc.add(p);
            }
        }
        return acc;
    }
}

/** Examples class to run our Masterpiece! */
class LectureExamples{
    /** Make a new one... */
    VoidWorld start = new EyecolorWorld();
    /** Start it up */
    VoidWorld end = start.bigBang();
}