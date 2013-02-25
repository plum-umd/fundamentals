import tester.*;

// CS 2510 - Spring 2012
// Assignment 4 Problem 2
// 8 February 2012

// to represent a point on canvas
class Posn{
  int x;
  int y;
  
  Posn(int x, int y){
    this.x = x; 
    this.y = y;
  }
}

// to represent a location in the game program
public class CartPt extends Posn{
  double doublex;
  double doubley;
  
  static CartPt ptN = new CartPt(0.0, -1.0);
  static CartPt ptS = new CartPt(0.0, 1.0);
  static CartPt ptE = new CartPt(1.0, 0.0);
  static CartPt ptW = new CartPt(-1.0, 0.0);
  
  static double sin45 = Math.sin(Math.PI/4);
  
  static CartPt ptNE = new CartPt(CartPt.sin45, -CartPt.sin45);
  static CartPt ptNW = new CartPt(-CartPt.sin45, -CartPt.sin45);
  static CartPt ptSW = new CartPt(-CartPt.sin45, CartPt.sin45);
  static CartPt ptSE = new CartPt(CartPt.sin45, CartPt.sin45);
  
  CartPt(Double doublex, Double doubley){
    super(doublex.intValue(), doubley.intValue());
    this.doublex = doublex;
    this.doubley = doubley;
  }
  
  CartPt(int x, int y){
    super(x, y);
    this.doublex = 1.0 * x;
    this.doubley = 1.0 * y;
  }
  
  // compute the distance from this point to the given point
  double distTo(CartPt that){
    return 
    Math.sqrt((this.doublex - that.doublex) * (this.doublex - that.doublex) +
              (this.doubley - that.doubley) * (this.doubley - that.doubley));
  }
  
  // produce the point on the unit circle in the same direction as this point
  CartPt unitDirectionTo(CartPt that){
    return new CartPt(that.doublex / this.distTo(that),
                      that.doubley / this.distTo(that));
  }
  
  // produce a String that describes the direction for traveling 
  // from this point to the given one
  String directionTo(CartPt that){
    
    if (CartPt.ptN.distTo(this.unitDirectionTo(that)) < 0.3)
      return "North";
    else if (CartPt.ptS.distTo(this.unitDirectionTo(that)) < 0.3)
      return "South";
    else if (CartPt.ptE.distTo(this.unitDirectionTo(that)) < 0.3)
      return "East";
    else if (CartPt.ptW.distTo(this.unitDirectionTo(that)) < 0.3)
      return "West";
    else if (CartPt.ptNE.distTo(this.unitDirectionTo(that)) < 0.3)
      return "NorthEast";
    else if (CartPt.ptNW.distTo(this.unitDirectionTo(that)) < 0.3)
      return "NorthWest";
    else if (CartPt.ptSE.distTo(this.unitDirectionTo(that)) < 0.3)
      return "SouthEast";
    else if (CartPt.ptSW.distTo(this.unitDirectionTo(that)) < 0.3)
      return "SouthWest";
    else
      return "Who knows";
  }
}

// examples and tests for the class CartPt
class ExamplesCartPt{
  ExamplesCartPt(){}
  
  CartPt p1 = new CartPt(0, 3);
  CartPt p2 = new CartPt(4, 3);
  CartPt p3 = new CartPt(4, 6);
  
  CartPt p00 = new CartPt(0, 0);
  CartPt pNorth = new CartPt(0, -5);
  CartPt pSouth = new CartPt(0, 5);
  CartPt pWest = new CartPt(-5, 0);
  CartPt pEast = new CartPt(5, 0);
  CartPt pNorthEast = new CartPt(5, -5);
  CartPt pSouthEast = new CartPt(5, 5);
  CartPt pNorthWest = new CartPt(-5, -5);
  CartPt pSouthWest = new CartPt(-5, 5);
  CartPt pwhoknows = new CartPt(5, 5);
  
  // test the method distTo in the class CartPt
  boolean testDistTo(Tester t){
    return
    t.checkExpect(this.p1.distTo(this.p2), 4.0) &&
    t.checkExpect(this.p1.distTo(this.p3), 5.0) &&
    t.checkExpect(this.p3.distTo(this.p2), 3.0);
  }
  
  // test the method directionTo in the class CartPt
  boolean testDirectionTo(Tester t){
    return
    t.checkExpect(this.p00.directionTo(this.pNorth), "North") &&
    t.checkExpect(this.p00.directionTo(this.pSouth), "South") &&
    t.checkExpect(this.p00.directionTo(this.pWest), "West") &&
    t.checkExpect(this.p00.directionTo(this.pEast), "East") &&
    t.checkExpect(this.p00.directionTo(this.pNorthEast), "NorthEast") &&
    t.checkExpect(this.p00.directionTo(this.pSouthWest), "SouthWest") &&
    t.checkExpect(this.p00.directionTo(this.pNorthWest), "NorthWest") &&
    t.checkExpect(this.p00.directionTo(this.pSouthWest), "SouthWest") &&
    t.checkExpect(this.p00.directionTo(this.pwhoknows), "Who knows");
  }
}