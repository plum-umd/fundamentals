// CS 2510 Spring 2012
// Assignment 3
// Problem 3.2
// 24 January 2012
// ExcelCells.java

import tester.*;

/*
          +------------+
+-------->| Cell       |<-------------------------+-+
|         +------------+                          | | 
|         | int row    |                          | | 
|         | int col    |                          | |
|         | IData data |-+                        | | 
|         +------------+ |                        | | 
|                      |                          | | 
|                      v                          | | 
|                  +-------+                      | | 
|                  | IData |                      | | 
|                  +-------+                      | | 
|                     / \                         | |
|                     ---                         | |
|                      |                          | | 
|     ---------------------------------           | |
|     |                 |             |           | | 
| +------------+   +-----------+  +----------+    | |
| | MyNumber   |   | Reference |  | Formula  |    | | 
| +------------+   +-----------+  +----------+    | |
| | int number | +-| Cell cell |  | Cell op1 |----+ |        
| +------------+ | +-----------+  | Cell op2 |------+
|                |              +-| IFun fun |
+----------------+              | +----------+            
                                v                    
                            +------+
                            | IFun |
                            +------+
                              / \
                              ---
                               |
           ---------------------------         
           |           |             |         
       +------+    +-------+    +-------+ 
       | Plus |    | Minus |    | Times | 
       +------+    +-------+    +-------+ 

 */

/*
  A sample spreadsheet:
  ---------------------

   |  A(1)    |  B(2)   |  C(3)   |  D(4)   |  E(5)     | 
---+----------+---------+---------+---------+-----------+ 
 1 |    7     |    4    |    3    |    2    |    5      | 
---+----------+---------+---------+---------+-----------+ 
 2 | mn A1 E1 | + B1 C1 | ref(A3) |         | * A2 D1   | 
---+----------+---------+---------+---------+-----------+ 
 3 |          | + B2 B1 |         |         | mn B3 D1  | 
---+----------+---------+---------+---------+-----------+ 
 4 |          | + B3 B2 |         |         | abs B4 D1 | 
---+----------+---------+---------+---------+-----------+ 
 5 |          | + B4 B3 |         |         | * A2 E4   | 
---+----------+---------+---------+---------+-----------+ 
 */

// to represent a cell in a spreadsheet
class Cell{
  int row;
  int col;
  IData data;

  Cell(int row, int col, IData data){
    this.row = row;
    this.col = col;
    this.data = data;
  }  

  /* TEMPLATE:
   FIELDS:
    ... this.row ...             -- int
    ... this.col ...             -- int
    ... this.data ...            -- IData

    METHODS:

    METHODS FOR FIELDS:

   */

}                                

// to represent data in one cell of a spreadsheet
interface IData{

}

// to represent numerical data in one cell of a spreadsheet
class MyNumber implements IData{
  int number;

  MyNumber(int number){
    this.number = number;
  }

  /* TEMPLATE:
   FIELDS:
    ... this.number ...             -- int

    METHODS:

   */

}

//to represent a reference to another cell in one cell of a spreadsheet
class Reference implements IData{
  Cell cell;

  Reference(Cell cell){
    this.cell = cell;
  }

  /* TEMPLATE:
   FIELDS:
   ... this.cell ...             -- Cell

   METHODS:

   METHODS FOR FIELDS:
   ... this.cell.???() ...            -- ??
  */

}

// to represent a formula data in one cell of a spreadsheet
class Formula implements IData{
  Cell op1;
  Cell op2;
  IFun fun;

  Formula(Cell op1, Cell op2, IFun fun){
    this.op1 = op1;
    this.op2 = op2;
    this.fun = fun;
  }


  /* TEMPLATE:
   FIELDS:
    ... this.op1 ...             -- Cell
    ... this.op2 ...             -- Cell
    ... this.fun ...             -- IFun

    METHODS:

    METHODS FOR FIELDS:
    ... this.op1.???() ...            -- ??
    ... this.op2.???() ...            -- ??
    ... this.fun.???() ...            -- ??
    
   */
 
}

// to represent a function of two arguments
interface IFun{
  
}                         

// to represent an addition function of two arguments
class Plus implements IFun{
  Plus(){}

  /* TEMPLATE:
   FIELDS:

    METHODS:

   */  

}

// to represent a subtraction function of two arguments
class Min implements IFun{
  Min(){}

  /* TEMPLATE:
   FIELDS:

    METHODS:

   */  

}   

// to represent a multiplication function of two arguments
class Times implements IFun{
  Times(){}

  /* TEMPLATE:
   FIELDS:

    METHODS:

   */  

}

// Examples and tests for classes that model a spreadsheet
class ExamplesExcelCells{
  ExamplesExcelCells(){}


  /*
  A sample spreadsheet:
  ---------------------

   |  A(1)    |  B(2)   |  C(3)   |  D(4)   |  E(5)     | 
---+-----------+---------+---------+---------+-----------+ 
 1 |    8      |    3    |    4    |    6    |    2      | 
---+-----------+---------+---------+---------+-----------+ 
 2 | mn A1 E1  | + B1 C1 |         |         | * B2 D1   | 
---+-----------+---------+---------+---------+-----------+ 
 3 | * A1 A2   | + B2 B1 |         |         | mn A3 D1  | 
---+-----------+---------+---------+---------+-----------+ 
 4 |           | + B3 B2 |         |         | mn B4 D1  | 
---+-----------+---------+---------+---------+-----------+ 
 5 |           | + B4 B3 |         |         | * B5 E4   | 
---+-----------+---------+---------+---------+-----------+ 

*/
}