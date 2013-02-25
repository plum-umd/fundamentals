/***********************************************
 *  CS2510 Spring 2011
 *  Lecture #9
 *  Constructor Overloading, Real Java,
 *    and Visibility
 ***********************************************/
import tester.*;

// Represents the Time
class Time{
    int hour;
    int min;
    int sec;

    // Complete constructor: verifies inputs
    Time(int hour, int min, int sec){
        if (0 <= hour && hour < 24)
            this.hour = hour;
        else
            throw new RuntimeException("Incorrect hours data");
        if (0 <= min && min < 60)
            this.min = min;
        else
            throw new RuntimeException("Incorrect minutes data");
        if (0 <= sec && sec < 60)
            this.sec = sec;
        else
            throw new RuntimeException("Incorrect seconds data");
    }

    // Initialize hours/min, with default of zero seconds
    Time(int hour, int min){
        this(hour, min, 0);
    }

    // Initialize hours/min/sec using AM/PM variant
    Time(int hour, int min, int sec, boolean am){
        this(hour, min, sec);

        // Adjust for the PM hours, but verify the hours again
        if (hour < 12 && !am)
            this.hour = hour + 12;
        else
            throw new RuntimeException("Incorrect hours data");
    }

    // Produce the time after the given hours and minutes have elapsed
    // Throws an exception if teh resulting time is the next day
    Time addTime(int hours, int minutes){
        if ((this.hour + hours + ((min + minutes) / 60)) > 23)
            throw new RuntimeException("Tomorrow");
        else
            return new Time(this.hour + hours + ((min + minutes) / 60),
                            (this.min + minutes) % 60,
                            this.sec);
    }
}

// Represents the Time, Part II
class Time2{
    // Seconds since midnight
    int sec;

    // We make this private, so *we* have the power
    private Time2(int sec){
        this.sec = sec;
    }

    // Time2 is now a direct replacement for Time1
    Time2(int hours, int min, int sec){
        this(hours * 3600 + min * 60 + sec);
    }
    // And, we can overload constructors as before (with defaults)
    Time2(int hours, int min){
        this(hours, min, 0);
    }

    /** Template
     *    Fields:
     *     ... this.sec ... -- int
     *
     *    Methods:
     *     ... this.asString() ... -- String
     */

    // Turn the time into a string
    String asString(){
        return ((this.sec/3600) + ":" +
                this.fixup((this.sec/60) % 60) + ":" +
                this.fixup(this.sec % 60));

    }

    // Private helper... turn "n" into a two digit string
    private String fixup(int n){
        if(n < 10){
            return "0"+n;
        }else{
            return ""+n;
        }
    }
}

// Examples for the class Time and the exception tests
class ExamplesTime{
    ExamplesTime(){}

    Time closing = new Time(22, 15, 0);
    Time closing1 = new Time(22, 15);
    Time closing2 = new Time(10, 15, 0, false);

    Time halfpastmid = new Time(0, 30, 0);
    //Time t1 = new Time(32, 12, 15);

    // Test constructors for the Time class
    // Still missing tests for the two more cases
    boolean testTime(Tester t){
        return (t.checkExpect(this.closing, this.closing1) &&
                t.checkExpect(this.closing, this.closing2) &&
                t.checkConstructorException(
                       // The test name - information about this test
                       "new Time(32, 20, 30) \n",
                                            
                       // The expected exception and message
                       new RuntimeException("Incorrect hours data"),
                                            
                       // The class where the constructor is defined
                       "Time",
                                            
                       // A comma-separated argument list (or no arguments)
                       32, 20, 30));
    }

    // Test the addTime method in the Time class
    boolean testAddTime(Tester t){
        return (t.checkExpect(this.closing.addTime(0, 75), new Time(23, 30, 0)) &&
                t.checkExpect(this.closing.addTime(1, 15), new Time(23, 30, 0)) &&
                t.checkException(
                       // The exception that should be thrown
                       new RuntimeException("Tomorrow"),
                       
                       // The object that invokes the method
                       this.closing,
                       
                       // The method name
                       "addTime",
                       
                       // The arguments for the method invocation
                       1, 50));
    }
    
    // Examples of Time2 (seconds based implementation
    Time2 den = new Time2(1, 35);
    Time2 err = new Time2(42, 64);
    
    // Test the asString method in the Time2 class
    boolean testAsString(Tester t){
        return (t.checkExpect(den.asString(), "1:35:00") &&
                t.checkExpect(err.asString(), "43:04:00"));
    }
}
