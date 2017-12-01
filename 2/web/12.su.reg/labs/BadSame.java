import tester.*;

// Test that illustrates incorrectly implemented equality
class BadSameExamples{
    BadSameExamples() {}
    
    I aA = new A(5);
    I aB = new B(5, 7.0);

    // The Tester knows the difference between these two objects
    boolean testSameTester(Tester t){
        // ** They are not the same...
        return (t.checkFail(aA, aB) &&
                t.checkFail(aB, aA));
    }
    
    // Check if the comparison works in one direction
    boolean testSame_BtoA(Tester t){
        // ** This test Passes... they are no tthe same
        return t.checkExpect(this.aB.same(this.aA), false, "Comparing B to A");
    }
    // Check if the comparison works in the other direction
    boolean testSame_AtoB(Tester t){
        // ** This test FAILS!! They are the same?!?!
        return t.checkExpect(this.aA.same(this.aB), false, "Comparing A to B");
    }
}

// A Simple interface
interface I{
    // Is this I the same as that I?
    public boolean same(I that);
}

// A Class that Implements I
class A implements I{
    int i;

    A(int i){
        this.i = i;
    }
    
    // Is this A the same as that I?
    public boolean same(I that){
        if(that instanceof A){
            return (this.sameA((A)that));
        }else{
            return false;
        }
    }
    // Is this A the same as that A?
    boolean sameA(A that){
        return (this.i == that.i);
    }
}

// A Class that Extends A
class B extends A{ 
    double d;

    B(int i, double d){
        super(i);
        this.d = d;
    }

    // Is this B the same as that I?
    public boolean same(I that){
        if(that instanceof B){
            return (this.sameB((B)that));
        }else{
            return false;
        }
    }
    // Is this B the same as that B?
    boolean sameB(B that){
        return (this.i == that.i &&
                this.d == that.d);
    }
}

/* Tester Output...
 ****************************
 * BadSameExamples:
 * ---------------
 *       new BadSameExamples:1(
 *          this.aA = 
 *            new A:2(
 *               this.i = 5)
 *          this.aB = 
 *            new B:3(
 *               this.i = 5
 *               this.d = 7.0))
 * ---------------
 * 
 * Ran 4 tests.
 * 1 test failed.
 * 
 * Failed test results: 
 * --------------
 * 
 * Error in test number 3
 * Comparing A to B
 * tester.ErrorReport: Error trace:
 * 	at BadSameExamples.testSame_AtoB(BadSame.java:19)
 * actual:                                 expected:
 * true .................................. false
 * 
 */