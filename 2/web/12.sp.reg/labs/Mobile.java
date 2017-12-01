// CS 2510 Spring 2012
// Lab 4

import tester.*;
/*
  Sample mobiles:
  ---------------
  
  Simple mobile      |
                     |
                     10
                    blue
  
  Complex mobile         |
                         |
                         |
             ------------+-----
             |                |
       ------+------          |
       |           |          | 
       10          |          40    
      red          10        green
                  blue
*/


/*
               +---------+                  
               | IMobile |<---------------+
               +---------+                |
               +---------+                |
                   |                      |
                  / \                     |
                  ---                     |
                   |                      |
         ---------------------            |
         |                   |            |
  +--------------+   +---------------+    |
  | Simple       |   | Complex       |    |
  +--------------+   +---------------+    |
  | int length   |   | int length    |    |
  | int weight   |   | int leftside  |    |
  | String color |   | int rightside |    |
  +--------------+   | IMobile left  |----+ 
                     | IMobile right |----+  
                     +---------------+ 
*/

// to represent a mobile
interface IMobile {

  // count the weights in this mobile
  int countWeights();

  // compute the total weight of this mobile
  int totalWeight();
  
  // compute the height of this mobile
  //int height();
  
  // is this mobile is balanced? (portfolio)
  //boolean isBalanced();

}

// to represent an item hanging at the end of a mobile
class Simple implements IMobile {
  int length;
  int weight;
  String color;

  Simple(int length, int weight, String color) {
    this.length = length;
    this.weight = weight;
    this.color = color;
  }

/* TEMPLATE:
   FIELDS:
    ... this.length ...         -- int
    ... this.weight ...         -- int
    ... this.color ...          -- String

    METHODS:
    ... this.countWeights() ...     -- int
    ... this.totalWeight() ...      -- int
    ... this.height() ...           -- int
    ... this.isBalanced() ...       -- boolean
*/

  // count the weights in this simple mobile
  int countWeights(){
    return 1;
  }
  // compute the total weight of this simple mobile
  int totalWeight(){
    return 0;
  }
  
}

// to represent a part of support structure for a mobile
class Complex implements IMobile {
  int length;
  int leftSide;
  int rightSide;
  IMobile left;
  IMobile right;

  Complex(int length, int leftSide, int rightSide, 
          IMobile left, IMobile right) {
    this.length = length;
    this.leftSide = leftSide;
    this.rightSide = rightSide;
    this.left = left;
    this.right = right;
  }

/* TEMPLATE:
   FIELDS:
    ... this.length ...           -- int
    ... this.leftside ...         -- int
    ... this.rightside ...        -- int
    ... this.left ...             -- IMobile
    ... this.right ...            -- IMobile

    METHODS:
    ... this.countWeights() ...     -- int
    ... this.totalWeight() ...      -- int
    ... this.height() ...           -- int
    ... this.isBalanced() ...       -- boolean
  
    METHODS FOR FIELDS:
    ... this.left.countWeights() ...     -- int
    ... this.left.totalWeight() ...      -- int
    ... this.left.height() ...           -- int
    ... this.left.isBalanced() ...       -- boolean

    ... this.right.countWeights() ...     -- int
    ... this.right.totalWeight() ...      -- int
    ... this.right.height() ...           -- int
    ... this.right.isBalanced() ...        -- boolean
*/

  // compute the total weight of this complex mobile
  int countWeights(){
    return this.left.countWeights() + 
           this.right.countWeights();
  }
  
  // compute the total weight of this complex mobile
  int totalWeight(){
    return 0;
  }
 
}

/*
  Sample mobiles:
  ---------------
  
  simple1:
                     |
                     |
                     10
                    blue
  
  complex2 (with complex1 on the left and simple2 on the right)
                 complex1 (with simple3 on the left, simple1 on the right)
  
                         |
                         |
                         |
             ------------+------
             |                 |
       ------+------           |
       |           |           | 
       10          |           40    
      red          10         green
                  blue
*/
class ExamplesMobiles {
  ExamplesMobiles(){}

  IMobile simple1 = new Simple(20, 10, "blue");
  IMobile simple2 = new Simple(30, 40, "green");
  IMobile simple3 = new Simple(10, 10, "red");

  IMobile complex1 = new Complex(10, 5, 5, this.simple3, this.simple1);
  IMobile complex2 = new Complex(30, 10, 5, this.complex1, this.simple2);
  
  // test the method totalWeight for the IMobile class hierarchy
  boolean testCountWeights(Tester t){
    return
    t.checkExpect(this.simple1.countWeights(), 1) &&
    t.checkExpect(this.complex1.countWeights(), 2) &&
    t.checkExpect(this.complex2.countWeights(), 3);
  }
}
