import java.util.*;
import tester.*;

/** Examples and tests for our parametrized list classes, our
 * <code>Traversal</code> wrapper for <code>ArrayList</code>s, and the
 * <code>Algorithms</code> class. 
 */
public class Examples{
  
    Examples(){}
  
    /** A big red <code>Balloon</code> */
    Balloon b1 = new Balloon(100, 200, 25, "red");
    
    /** A small blue <code>Balloon</code> */
    Balloon b2 = new Balloon(200, 100, 20, "blue");
  
    /** A big green <code>Balloon</code> */
    Balloon b3 = new Balloon(200, 150, 30, "green");
  
    /** A small red <code>Balloon</code> */
    Balloon b4 = new Balloon(200, 120, 22, "red");

    IList<Balloon> mt = new MtList<Balloon>();
  
    /** A list with red and small Balloons */
    IList<Balloon> blist = new ConsList<Balloon>(this.b1,
         new ConsList<Balloon>(this.b2,
             new ConsList<Balloon>(this.b3,
                 new ConsList<Balloon>(this.b4, this.mt))));

    /** A list with no red Balloons */
    IList<Balloon> blistNoRed = new ConsList<Balloon>(this.b2,
         new ConsList<Balloon>(this.b3, this.mt));
 
    /** A list with no small Balloons */
    IList<Balloon> blistNoSmall = new ConsList<Balloon>(this.b1,
         new ConsList<Balloon>(this.b3, this.mt));
  
    /** Constructs an <code>ArrayList</code> with four <code>Balloon</code>s */
    ArrayList<Balloon> makeArrBlist(){
        ArrayList<Balloon> arrlist = new ArrayList<Balloon>();
        arrlist.add(this.b1);
        arrlist.add(this.b2);
        arrlist.add(this.b3);
        arrlist.add(this.b4);
        return arrlist;
    }
  
    /** Constructs an <code>ArrayList</code> with no red <code>Balloon</code>s */
    ArrayList<Balloon> makeArrBlistNoRed(){
        ArrayList<Balloon> arrlist = new ArrayList<Balloon>();
        arrlist.add(this.b2);
        arrlist.add(this.b3);
        return arrlist;
    }
 
    /** Constructs an <code>ArrayList</code> with no small <code>Balloon</code>s */
    ArrayList<Balloon> makeArrBlistNoSmall(){
        ArrayList<Balloon> arrlist = new ArrayList<Balloon>();
        arrlist.add(this.b1);
        arrlist.add(this.b3);
        return arrlist;
    }
    
    /** a <code>Traversal</code> for <code>ArrayList</code>
     * with four <code>Balloon</code>s */
    ALTrav<Balloon> arrblist = 
        new ALTrav<Balloon>(this.makeArrBlist());

    /** a <code>Traversal</code> for <code>ArrayList</code>
     * with no red <code>Balloon</code>s */
    ALTrav<Balloon> arrblistNoRed = 
        new ALTrav<Balloon>(this.makeArrBlistNoRed());

    /** a <code>Traversal</code> for <code>ArrayList</code>
     * with no small <code>Balloon</code>s */
    ALTrav<Balloon> arrblistNoSmall = 
        new ALTrav<Balloon>(this.makeArrBlistNoSmall());
  
    /** Balloon class tests: equality */  
    void testEquality(Tester t){   
        t.checkExpect(this.b2, new Balloon(200, 100, 20, "blue"), 
                      "the same Balloons OK");
        t.checkExpect(this.b2, this.b2, "the same Balloons OK");
    }
       
    /** Balloon class tests: the distanceFromTop method */ 
    void testDistanceFromTop(Tester t){   
        t.checkExpect(this.b1.distanceFromTop(), 200 - 25, "distanceFromTop 1");    
        t.checkExpect(this.b2.distanceFromTop(), 80, "distanceFromTop 2");  
    }
  
    /** A predicate function object for red <code>Balloon</code>s */
    ISelect<Balloon>  redBalloon = new RedBalloon();
  
    /** A predicate function object for red <code>Balloon</code>s */
    ISelect<Balloon> smallBalloon = new SmallBalloon(23);
  
    /** ISelect classes tests: RedBalloon and SmallBalloon */ 
    void testSelectors(Tester t){
        t.checkExpect(this.redBalloon.select(this.b1), true, "red Balloon - true");
        t.checkExpect(this.redBalloon.select(this.b2), false, "red Balloon - false");
    
        t.checkExpect(this.smallBalloon.select(this.b2), true, "small Balloon - true");
        t.checkExpect(this.smallBalloon.select(this.b1), false, "small Balloon - false");
    }
  
    /** An instance of the <code>Algorithms</code> class */
    Algorithms algo = new Algorithms();
  
    /** Algorithms methods tests: contains */ 
    void testContains(Tester t){
        t.checkExpect(this.algo.contains(this.blist, this.redBalloon), true);
        t.checkExpect(this.algo.contains(this.blistNoRed, this.redBalloon), false);
        t.checkExpect(this.algo.contains(this.blist, this.smallBalloon), true);
        t.checkExpect(this.algo.contains(this.blistNoSmall, this.smallBalloon), false);
    
        t.checkExpect(this.algo.contains(this.arrblist, this.redBalloon), true);
        t.checkExpect(this.algo.contains(this.arrblistNoRed, this.redBalloon), false);
        t.checkExpect(this.algo.contains(this.arrblist, this.smallBalloon), true);
        t.checkExpect(this.algo.contains(this.arrblistNoSmall, this.smallBalloon), false);
    }

    /** Algorithms methods tests: countSuch */ 
    void testCountSuch(Tester t){
        t.checkExpect(this.algo.countSuch(this.blist, this.redBalloon), 2);
        t.checkExpect(this.algo.countSuch(this.blist, this.smallBalloon), 2);

        t.checkExpect(this.algo.countSuch(this.arrblist, this.redBalloon), 2);
        t.checkExpect(this.algo.countSuch(this.arrblist, this.smallBalloon), 2);
    }
  
    /** A ForEach for 'contains red Balloon' */
    ForEach<Balloon, Boolean> containsRedBlist = 
        new ForEach<Balloon, Boolean>(false,  new OrSelectUpdater(this.redBalloon), this.blist);

    /** A ForEach for 'contains small Balloon' */
    ForEach<Balloon, Boolean> containsSmallBlist = 
        new ForEach<Balloon, Boolean>(false, new OrSelectUpdater(this.smallBalloon), this.blist);
 
    /** A ForEach for 'contains small Balloon' */
    ForEach<Balloon, Boolean> containsSmallArrlist = 
        new ForEach<Balloon, Boolean>(false, new OrSelectUpdater(this.smallBalloon), this.arrblistNoSmall);
     
    /** ForEach method tests: contains */ 
    public void testContainsForEach(Tester t){
        t.checkExpect(this.containsRedBlist.loopRec(), true);
        t.checkExpect(this.containsRedBlist.loopWhile(), true);
        t.checkExpect(this.containsRedBlist.loopFor(), true);
    
        t.checkExpect(this.containsSmallBlist.loopRec(), true);
        t.checkExpect(this.containsSmallBlist.loopWhile(), true);
        t.checkExpect(this.containsSmallBlist.loopFor(), true);

        t.checkExpect(this.containsSmallArrlist.loopRec(), false);
        t.checkExpect(this.containsSmallArrlist.loopWhile(), false);
        t.checkExpect(this.containsSmallArrlist.loopFor(), false);
    }   
}
