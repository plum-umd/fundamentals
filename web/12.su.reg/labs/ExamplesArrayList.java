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
	}
	
	/**
	 * EFFECT:
	 * Run the tests to validate the design of the <code>ArrayList</code> class.
	 * 
	 * @param t The tester that runs the tests and reports the results
	 */
	public void testArrayList(Tester t){
		this.initList();
		t.checkExpect(this.arlist.size(), 4);		
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