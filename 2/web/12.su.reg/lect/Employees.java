// CS U2510 Fall 2011
// Lab 4: Employees.java

/*
 *                      +-----------+
 *                      |    Emp    |<-----------------------+
 *                      +-----+-----+                        |
 *                           / \                             |
 *                          +---+                            |
 *                            |                              |
 *            +---------------+-----------------+            |
 *            |                                 |            |
 *    +-------+--------+                 +------+---------+  |
 *    |    Worker      |                 |    Boss        |  |
 *    +----------------+                 +----------------+  |
 *    | String name    |                 | String name    |  |
 *    | Number tasks  |                 | String unit    |  |
 *    +----------------+                 | Number tasks  |  |
 *                              +--------| ILoE   peons   |  |
 *                              |        +----------------+  |
 *                              V                            |
 *                        +--------------+                   |
 *                        |     ILoE     |<------------------+--+
 *                        +------+-------+                   |  |
 *                              / \                          |  |
 *                             +---+                         |  |
 *                               |                           |  |
 *                +--------------+--------------+            |  |
 *                |                             |            |  |
 *        +-------+------+              +-------+-------+    |  |
 *        |     MtLoE    |              |    ConsLoE    |    |  |
 *        +--------------+              +---------------+    |  |
 *        +--------------+              | Emp  first    |----+  |
 *                                      | ILoE rest     |-------+
 *                                      +---------------+
 */

// Represents an Employee of a company
interface Emp{
}

// Represents an employee without subordinates
class Worker implements Emp{
    String name;
    Number tasks;

    Worker(String name, Number tasks){
        this.name = name;
        this.tasks = tasks;
    }

    /* Template:
     *   Fields:
     *     ... this.name ...   -- String
     *     ... this.tasks ...  -- int
     *
     *    Methods:
     */
}

// Represents an boss in charge of a unit, with a list of subordinates
class Boss implements Emp {
    String name;
    String unit;
    Number tasks;
    ILoE peons;

    Boss(String name, String unit, Number tasks, ILoE peons){
        this.name = name;
        this.unit = unit;
        this.tasks = tasks;
        this.peons = peons;
    }

    /* Template:
     *    Fields:
     *      ... this.name ...    -- String
     *      ... this.unit ...    -- String
     *      ... this.tasks ...   -- int
     *      ... this.peons ...   -- ILoE
     *
     *    Methods:
     *
     *    Methods for Fields:
     */
}

// Represents a list of employees
interface ILoE{
}

// Represents an empty list of employees
class MtLoE implements ILoE{
    MtLoE(){}
}

// Represents a list of employees
class ConsLoE implements ILoE {
    Emp first;
    ILoE rest;

    ConsLoE(Emp first, ILoE rest){
        this.first = first;
        this.rest = rest;
    }

    /* Template:
     *   Fields:
     *     ... this.first ...   -- Emp
     *     ... this.rest ...    -- ILoE
     *
     *   Methods:
     *
     *   Methods for Fields:
     */

}

// Examples/tests for the Employee hierarchy
class EmployeeExamples{
    EmployeeExamples(){ }
    
    Emp wkA = new Worker("Allen",3);
    Emp wkB = new Worker("Bob",5);
    Emp wkC = new Worker("Carrie",6);
    Emp wkD = new Worker("Damion",4);
    Emp wkE = new Worker("Ersela",5);
    Emp wkF = new Worker("Frank",7);

    ILoE mtlist = new MtLoE();

    ILoE grpAlist = new ConsLoE(this.wkA, this.mtlist);
    ILoE secAlist = new ConsLoE(this.mike,
                          new ConsLoE(this.wkB,
                                new ConsLoE(this.wkE,this.mtlist)));
    ILoE secBlist = new ConsLoE(this.wkC,
                          new ConsLoE(this.wkD, this.mtlist));
    ILoE secClist = new ConsLoE(this.wkE, this.mtlist);

    ILoE operList = new ConsLoE(this.jack,
                          new ConsLoE(this.jenn, this.mtlist));

    ILoE financeList =  new ConsLoE(this.wkF,
                              new ConsLoE(this.pat, this.mtlist));

    ILoE ceoList = new ConsLoE(this.dave,
                         new ConsLoE(this.anne,this.mtlist));

    Emp mike = new Boss("Mike", "Group A", 10, this.grpAlist);
    Emp jack = new Boss("Jack", "Section A", 25, this.secAlist);
    Emp jenn = new Boss("Jenn", "Section B", 15, this.secBlist);
    Emp pat  = new Boss("Pat",  "Section C", 20, this.secClist);
    Emp dave = new Boss("Dave", "Operations", 70, this.operList);
    Emp anne = new Boss("Anne", "Finance", 20, this.financeList);
    Emp meg  = new Boss("Meg","CEO", 100, this.ceoList);
}