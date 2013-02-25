import tester.*;

/*
Make examples of data - including a gift with at least three wraps 
   
Design the method that counts the wraps for a gift

Design the method that determines whether a gift contains
 the given jewel in the given box?

Design the method that will produce a new gift, same as before, 
 but with the given jewel inside
*/

// to represent a gift of a jewel in a box and wrapped several times
interface IGift{

  // count the wraps for this gift?
  public int countWraps();
  
  //is this a gift with the given jewel and box?
  public boolean sameJewelBox(String jewel, String box); 

  // make a new gift just like this one but with the given jewel inside
  public IGift newJewel(String jewel);    
}

// to represent the jewel inside the gift box
class Jewel{
  String name;
  
  Jewel(String name){
    this.name = name;
  }
  
  // does this jewel have the given name?
  public boolean sameJewel(String name){
    return this.name.equals(name);
  }
}

// to represent the gift box with a jewel inside
class Box implements IGift{
  String kind;
  Jewel jewel;
  
  Box(String kind, Jewel jewel){
    this.kind = kind;
    this.jewel = jewel;
  }

  //count the wraps for this gift?
  public int countWraps(){
    return 0;
  }
  
  //is this a a gift with the given jewel and box?
  public boolean sameJewelBox(String jewel, String box){
    if (this.kind.equals(box))
      return this.jewel.sameJewel(jewel);
    else
      return false;
  } 

  // make a new gift just like this one but with the given jewel inside
  public IGift newJewel(String jewel){
    return new Box(this.kind, new Jewel(jewel));
  }
}

// to represent a gift of a jewel in a box, wrapped several times
class Wrap implements IGift{
  IGift gift;
  
  Wrap(IGift gift){
    this.gift = gift;
  }

  //count the wraps for this gift?
  public int countWraps(){
    return 1 + this.gift.countWraps();
  }
  
  //is this a a gift with the given jewel and box?
  public boolean sameJewelBox(String jewel, String box){
    return
    this.gift.sameJewelBox(jewel, box);
  }

  // make a new gift just like this one but with the given jewel inside
  public IGift newJewel(String jewel){
    return new Wrap(this.gift.newJewel(jewel));
  }
}

// examples and tests for the class hierarchy that represents the gifts
class ExamplesIGift{
  ExamplesIGift(){}
  
  Jewel ruby = new Jewel("Ruby");
  Jewel garnet = new Jewel("Garnet");
  
  IGift redbox = new Box("Red Box", this.ruby);
  IGift redboxGarnet = new Box("Red Box", this.garnet);
  
  IGift tissue = new Wrap(this.redbox);
  IGift bluewrap = new Wrap(tissue);
  IGift ribbon = new Wrap(this.bluewrap);
  
  
  //test the method countWraps for the IGift class hierarchy
  boolean testCountWraps(Tester t){
    return
    t.checkExpect(this.redbox.countWraps(), 0) &&
    t.checkExpect(this.tissue.countWraps(), 1) &&
    t.checkExpect(this.bluewrap.countWraps(), 2) &&
    t.checkExpect(this.ribbon.countWraps(), 3);
  }
  
  // test the method sameJewelBox for the IGift class hierarchy
  boolean testSameJewelBox(Tester t){
    return
    t.checkExpect(this.redbox.sameJewelBox("Ruby", "Red Box"), true) &&
    t.checkExpect(this.tissue.sameJewelBox("Ruby", "Blue Box"), false) &&
    t.checkExpect(this.tissue.sameJewelBox("Opal", "Red Box"), false) &&
    t.checkExpect(this.ribbon.sameJewelBox("Ruby", "Red Box"), true) &&
    t.checkExpect(this.bluewrap.sameJewelBox("Ruby", "Blue Box"), false) &&
    t.checkExpect(this.ribbon.sameJewelBox("Opal", "Red Box"), false);
  }
  
  // test the method sameJewel for the Jewel class
  boolean testSameJewel(Tester t){
    return
    t.checkExpect(this.ruby.sameJewel("Ruby"), true) &&
    t.checkExpect(this.ruby.sameJewel("Opal"), false);
  }
  
  // test the method newJewel for the IGift class hierarchy
  boolean testNewJewel(Tester t){
    return
    t.checkExpect(this.redbox.newJewel("Garnet"), this.redboxGarnet) &&
    t.checkExpect(this.tissue.newJewel("Garnet"), 
        new Wrap(this.redboxGarnet)) &&
    t.checkExpect(this.ribbon.newJewel("Garnet"), 
        new Wrap(new Wrap(new Wrap(this.redboxGarnet))));
  }
}


/*
                       +-------+
                       | IGift |<--------------+
                       +-------+               |
                          / \                  |
                          ---                  |
                           |                   |
                   -------------------         |
                   |                 |         |
             +--------------+  +------------+  |
             | Box          |  | Wrap       |  |
             +--------------+  +------------+  |
         +---| String kind  |  | IGift gift |--+ 
         |   | Jewel inside |  +------------+
         |   +--------------+ 
         v
 +--------------+
 | Jewel        |
 +--------------+
 | String jewel |
 +--------------+
 
Jewel diamond = new Jewel("Diamond");
IGift inBox = new Box("Oval", this.diamond);
IGift tissue = new Wrap(this.inBox);
IGift blue = new Wrap(this.tissue);
IGift gold = new Wrap(this.blue);

*/