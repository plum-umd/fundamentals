import tester.*;
import java.util.*;

/**
 * A class designed to explore and validate the methods defined in the 
 * <code>ArrayList</code> class.
 * 
 * @since 13 March 2012
 */
class ExamplesArrayList{
  ExamplesArrayList(){}
  
  /** A sample <code>ArrayList</code> */
  ArrayList<Person> arlist = new ArrayList<Person>();
  
  ArrayList<Person> arlistSame = arlist;
  ArrayList<Person> arlistOther;
  
  Person pete = new Person("Pete", 23);
  Person jill = new Person("Jill", 19);
  Person kim = new Person("Kim", 20);
  Person pat = new Person("Pat", 21);
  
  /**
   * Test the method newAge for the class Person
   *
   * @param t The tester that runs the tests and reports the results
   */
  public void testNewAge(Tester t){
    t.checkExpect(this.jill.newAge(25), new Person("Jill", 25));
  }
  
  /**
   * Test the method birthday for the class Person
   *
   * @param t The tester that runs the tests and reports the results
   */
  public void testBirthday(Tester t){
    // invoke the method
    this.jill.birthday();
    
    // verify the effects
    t.checkExpect(this.jill, new Person("Jill", 20));
    
    // reset
    this.jill.age = 19;
  }
  
  /**
   * EFFECT:
   * Initialize the array list with four data items
   */
  public void initList(){
    this.arlist.clear();
    this.arlist.add(this.pete);
    this.arlist.add(this.jill);
    this.arlist.add(this.kim);
    this.arlist.add(this.pat);
    this.arlistOther = new ArrayList<Person>(this.arlist); 
  }
  
  /**
   * EFFECT:
   * Run the tests to validate the design of the <code>ArrayList</code> class.
   * 
   * @param t The tester that runs the tests and reports the results
   */
  public void testArrayList(Tester t){
    // check the initial values
    t.checkExpect(this.arlist.size(), 0);
    t.checkExpect(this.arlistSame.size(), 0);
    t.checkExpect(this.arlistOther, null);
    
    // initialize the lists and check the effects
    this.initList();
    t.checkExpect(this.arlist.size(), 4);
    t.checkExpect(this.arlistSame.size(), 4);
    t.checkExpect(this.arlistOther.size(), 4);
    
    t.checkExpect(this.arlist.get(0), this.pete);
    t.checkExpect(this.arlistSame.get(0), this.pete);
    t.checkExpect(this.arlistOther.get(0), this.pete);

    // verify that the tests have the same data
    t.checkExpect(this.arlist, this.arlistSame);
    t.checkExpect(this.arlist, this.arlistOther);
    
    
    Person j = this.arlist.set(1, new Person("Jack", 20));
    t.checkExpect(j, this.jill);
    t.checkExpect(this.arlist.get(1), new Person("Jack", 20));
    t.checkExpect(this.arlistSame.get(1), new Person("Jack", 20));
    t.checkExpect(this.arlistOther.get(1), new Person("Jill", 19));
    
    t.checkExpect(this.arlist, this.arlistSame);
    
    ArrayList<Person> arlist2 = arlist;
    Person x = arlist2.remove(3);

    t.checkExpect(x, this.pat);
    t.checkExpect(arlist.size(), 3);
    t.checkExpect(arlist2.size(), 3);
    t.checkExpect(this.arlistOther.size(), 4);
    
    t.checkExpect(this.arlist, this.arlistSame);
    
  }
  
  public <T> void swapAtIndices(ArrayList<T> alist, int index1, int index2){
    //alist.set( index1, alist.set( index2, alist.get( index1 ) ) );
    T old = alist.get(index1);
    alist.set(index1, alist.get(index2));
    alist.set(index2, old);
  }

  /**
   * EFFECT:
   * Run the tests to validate the design of the <code>ArrayList</code> class.
   * 
   * @param t The tester that runs the tests and reports the results
   */
  public void testSwapAtIndices(Tester t){
    initList();
    t.checkExpect(this.arlist.get(2), this.kim);
    t.checkExpect(this.arlist.get(3), this.pat);
    t.checkExpect(this.arlistOther.get(2), this.kim);
    t.checkExpect(this.arlistOther.get(3), this.pat);
    
    this.swapAtIndices(this.arlist, 3, 2);

    t.checkExpect(this.arlist.get(3), this.kim);
    t.checkExpect(this.arlist.get(2), this.pat);
    t.checkExpect(this.arlistOther.get(2), this.kim);
    t.checkExpect(this.arlistOther.get(3), this.pat);
    
    t.checkExpect(this.arlist.get(2), this.arlistOther.get(3));
    t.checkExpect(this.arlist.get(3), this.arlistOther.get(2));
  }
  
  
  /**
   * Run the tests defined in the <code>ExamplesArrayList</code> class.
   * 
   * @param argv unused
   */
  public static void main(String[] argv){
    ExamplesArrayList e = new ExamplesArrayList();
    // run tests and report the results: print all test results, print all data
    Tester.runReport(e, true, true);
  }
}

/**
 * A class of objects to include in the sample ArrayList
 */
class Person{
  
  /** the name of this person */
  String name;
  
  /** the age of this person */
  int age;
  
  /**
   * The full constructor
   * @param name the name of this person
   * @param age the age of this person
   */
  Person(String name, int age){
    this.name = name;
    this.age = age;
  }
  
  /**
   * Produce a new person with the same name as this one, 
   * but with the given age
   * 
   * @param age the given age
   * @return a new pesron with the same name as this one, but with the given age
   */
  public Person newAge(int age){
    return new Person(this.name, age);
  }
  
  /**
   * EFFECT:
   * change this person's age to one year older when she reached her birthday 
   */
  public void birthday(){
    this.age = this.age + 1;
  }
}