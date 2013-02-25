/***********************************************
 *  CS2510 Spring 2011 
 *  Lecture #28-ish
 *  Sorting Demonstration(s)
 ***********************************************/

import tester.*;

/** Represents an Expression in our "<i>language</i>" */
interface Expr{
    
}

/** Represents a binary expression
 *  (e.g., addition/subtraction/multiplication) */
abstract class Bin implements Expr{
    Expr left;
    Expr right;
    
    Bin(Expr left, Expr right){
        this.left = left;
        this.right = right;
    }
}

/** Represents a multiplication expression,
 *    e.g.: (20*y) */
class Mult extends Bin{
    
    Mult(Expr left, Expr right){
        super(left, right);
    }
}

/** Represents an addition expression,
 *    e.g.: (x+10) */
class Sum extends Bin{
    
    Sum(Expr left, Expr right){
        super(left, right);
    }
}

/** Represents a variable expression,
 *    e.g.: x */
class Var implements Expr{
    String name;
    
    Var(String name){
        this.name = name;
    }
}

/** Represents an integer value/expression,
 *    e.g.: 45 */
class Val implements Expr{
    int i;
    
    Val(int i){
        this.i = i;
    }
}

/** Represents an equality/definition/assignment,
 *    e.g.: y = (2*(x*x)) */
class Equation{
    Var v;
    Expr e;
    
    Equation(Var v, Expr e){
        this.v = v;
        this.e = e;
    }
}

/** Represents an arbitrary equation of expressions,
 *    e.g.: (x*3) = (y+45)*/
class JoshEquation{
    Expr el;
    Expr er;
    
    JoshEquation(Expr el, Expr er){
        this.el = el;
        this.er = er;
    }
}


/** Examples/Tests */
class LectureExamples{
    LectureExamples(){}

    Expr ten = new Val(10);
    Expr twenty = new Val(20);
    
    Var x = new Var("x");
    Var y = new Var("y");
    Var z = new Var("z");
    
    Expr e1 = new Sum(new Mult(this.x, this.y),
                      new Val(4));
    
    Equation eq1 = new Equation(this.x, this.ten);
    Equation eq2 = new Equation(this.y, this.twenty);
    
    JoshEquation ejosh = new JoshEquation(this.e1, this.e1);
}
