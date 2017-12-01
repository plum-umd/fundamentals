import tester.*;

// Represents a date
// Month has to be in [1,12], Day in [1,31].
class Date {
    Integer year;
    Integer month;
    Integer day;
    Date(Integer year, Integer month, Integer day) {
	this.year = year;
	if (month > 12) {
	    throw new RuntimeException("There are only 12 months on this planet.");
	}
	this.month = month;
	if (day > 31) {
	    throw new RuntimeException("There are never more than 31 days in a month.");
	}
	this.day = day;
    }

    private Date() {
	this.year = 1970;
	this.month = 1;
	this.day = 1;
    }

    Date blah() {
	return new Date();
    }

    Date(String s) {
	this();
	if (s.equals("My birthday")) {
	    this.year = 2002;
	    this.month = 2;
	    this.day = 2;
	} else {

	}
    }
}

class Examples {
    Date d1 = new Date(1992, 11, 1);
    // Date d2 = new Date(-45, 34, 783);  // Reject
    Date d3 = new Date(d1);
    Date d4 = new Date("My birthday");
    Date d5 = new Date("Not my birthday");
}