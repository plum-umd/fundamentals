import tester.*;

/*
Make examples of data - including a pizza with at least three toppings 
   
Design the method that counts the number of toppings on a pizza

Design the method that determines whether the pizza
 has the given crust and cheese?

Design the method that will produce a new pizza, same as before, 
 but with the given crust instead
*/

interface IPizza{
  
  // count the toppings on this pizza
  public int countToppings();
  
  // is this a pizza with the given crust and cheese?
  public boolean sameCrustCheese(String crust, String cheese);   
  
  // make a new pizza just like this one but with the given crust
  public IPizza newCrust(String crust);
}

class Crust{
  String name;
  
  Crust(String name){
    this.name = name;
  }
  
  // is this a pizza with the given crust?
  public boolean sameCrust(String crust){
    return this.name.equals(crust);
  }
}


class Cheese implements IPizza{
  String kind;
  Crust crust;
  
  Cheese(String kind, Crust crust){
    this.kind = kind;
    this.crust = crust;
  }

  // count the toppings on this pizza
  public int countToppings(){
    return 0;
  }
  
  // is this a pizza with the given crust and cheese?
  public boolean sameCrustCheese(String crust, String cheese){
    if (this.kind.equals(cheese))
      return this.crust.sameCrust(crust);
    else
      return false;
  }  
  
  // make a new pizza just like this one but with the given crust
  public IPizza newCrust(String crust){
    return new Cheese(this.kind, new Crust(crust));
  }
}

class Topping implements IPizza{
  IPizza base;
  
  Topping(IPizza base){
    this.base = base;
  }

  // count the toppings on this pizza
  public int countToppings(){
    return 1 + this.base.countToppings();
  }
  
  // is this a pizza with the given crust and cheese?
  public boolean sameCrustCheese(String crust, String cheese){
    return
    this.base.sameCrustCheese(crust, cheese);
  }  
  
  // make a new pizza just like this one but with the given crust
  public IPizza newCrust(String crust){
    return new Topping(this.base.newCrust(crust));
  }
}

// examples and tests for the IPizza class hierarchy
class ExamplesIPizza{
  ExamplesIPizza(){}
  
  Crust deepdish = new Crust("Deep Dish");
  Crust thin = new Crust("Thin Crust");
  
  IPizza threecheese = new Cheese("Three cheese", this.deepdish);
  IPizza threecheeseThin = new Cheese("Three cheese", this.thin);
  
  IPizza anchovy = new Topping(this.threecheese);
  IPizza onion = new Topping(anchovy);
  IPizza olive = new Topping(this.onion);
  
  // test the method countToppings for the IPizza class hierarchy
  boolean testCountToppings(Tester t){
    return
    t.checkExpect(this.threecheese.countToppings(), 0) &&
    t.checkExpect(this.anchovy.countToppings(), 1) &&
    t.checkExpect(this.onion.countToppings(), 2) &&
    t.checkExpect(this.olive.countToppings(), 3);
  }
  
  // test the method sameCrustCheese for the IPizza class hierarchy
  boolean testSameCrustCheese(Tester t){
    return
    t.checkExpect(this.threecheese.sameCrustCheese("Deep Dish", "Three cheese"), 
        true) &&
    t.checkExpect(this.threecheese.sameCrustCheese("Deep Dish", "Mozarella"), 
        false) &&
    t.checkExpect(this.threecheese.sameCrustCheese("Thin Crust", "Three cheese"), 
        false) &&
    t.checkExpect(this.onion.sameCrustCheese("Deep Dish", "Three cheese"), 
        true) &&
    t.checkExpect(this.anchovy.sameCrustCheese("Deep Dish", "Mozarella"), 
        false) &&
    t.checkExpect(this.onion.sameCrustCheese("Thin Crust", "Three cheese"), 
        false);
  }
  
  // test the method sameCrust for the Crust class
  boolean testSameCrust(Tester t){
    return
    t.checkExpect(this.deepdish.sameCrust("Deep Dish"), true) &&
    t.checkExpect(this.deepdish.sameCrust("Thin crust"), false);
  }
  
  // test the method newCrust for the IPizza class hierarchy
  boolean testNewCrust(Tester t){
    return
    t.checkExpect(this.threecheese.newCrust("Thin Crust"), 
        this.threecheeseThin) &&
    t.checkExpect(this.onion.newCrust("Thin Crust"), 
        new Topping(new Topping(this.threecheeseThin)))&&
    t.checkExpect(this.anchovy.newCrust("Thin Crust"), 
        new Topping(this.threecheeseThin)) ;
  }
}


/*
                +--------+
                | IPizza |<-----------------------+
                +--------+                        |
                           / \                    |
                           ---                    |
                            |                     |
                     -------------------          |
                     |                 |          |
               +-------------+   +-------------+  |
               | Cheese      |   | Topping     |  |
               +-------------+   +-------------+  |
       +-------| String kind |   | IPizza base |--+ 
       |       | Crust crust |   +-------------+
       |       +-------------+
       v
 +-------------+ 
 | Crust       |
 +-------------+
 | String name |
 +-------------+ 

Crust deepdish = new Crust("Deep Dish");
IPizza threecheese = new Cheese("Three cheese", this.deepdish);
IPizza anchovy = new Topping(this.threecheese);
IPizza onion = new Topping(anchovy);
IPizza olive = new Topping(this.onion);

*/