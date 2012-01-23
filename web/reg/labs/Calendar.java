import tester.*;

// CS 2510 Spring 2012
// Program to use for Lab 3
// Author: Viera K. Proulx
// Date: 20 January 2012

// Student pair XXX:
// name 1
// name 2

// to represent a time of day in hours and minutes
class ClockTime{
	int minutes;  // must be in the range [0, 24)
	int hours;    // must be in the range [0, 60)
	
	ClockTime(int hours, int minutes){
		this.hours = hours;
		this.minutes = minutes;
	}
	
  /* TEMPLATE
   * FIELDS:
   * ... this.hours ...         -- int
   * ... this.minutes ...       -- int
   * METHODS:
   * ... this.toMinutes() ...   -- int
   * ...
   */
	
	// convert the time on this clock to total minutes since midnight
	public int toMinutes(){
		return 0;
	}
}

// to represent an event in a calendar
class Event{
	String name;
	ClockTime startTime;
	ClockTime endTime;
	
	Event(String name, ClockTime startTime, ClockTime endTime){
		this.startTime = startTime;
    this.endTime = endTime;
	}
	
	/* TEMPLATE
	 * FIELDS:
	 * ... this.name ...                    -- String
	 * ... this.startTime ...               -- ClockTime
   * ... this.endTime ...                 -- ClockTime
	 * METHODS:
	 * ... this.duration() ...              -- int
	 * ...
	 * METHODS FOR FIELDS:
	 * ... this.startTime.toMinutes() ...   -- int
   * ... this.endTime.toMinutes() ...     -- int
	 */	
	
	// compute the duration of this event in minutes
	public int duration(){
	  return 0;
	}
	
}

// to represent events scheduled for one day
interface ISchedule{
  
  // compute the total scheduled time for this day
  public int scheduledTime();
  
}

class NoEvents implements ISchedule{
  NoEvents(){}
  
  // compute the total scheduled time for this day
  public int scheduledTime(){
    return 0;
  }
}

class ConsEvents implements ISchedule{
  Event first;
  ISchedule rest;
  
  ConsEvents(Event first, ISchedule rest){
    this.first = first;
    this.rest = rest;
  }

  /* TEMPLATE
   * FIELDS:
   * ... this.first ...                   -- Event
   * ... this.rest ...                    -- ISchedule
   * METHODS:
   * ... this.scheduledTime() ...         -- int
   * ...
   * METHODS FOR FIELDS:
   * ... this.first.duration() ...       -- int
   * ... this.rest.scheduledTime() ...    -- int
   */ 
  
  // compute the total scheduled time for this day
  public int scheduledTime(){
    return 0;
  }
}

class ExamplesCalendar{
  ExamplesCalendar(){}
  
  ClockTime tenAM = new ClockTime(10, 0);
  ClockTime eleven30 = new ClockTime(11, 30);
  
  Event fundies2 = new Event("Fundies 2", this.tenAM, this.eleven30);
  
  ISchedule allFree = new NoEvents();
  ISchedule onlyFundies2 = new ConsEvents(this.fundies2, this.allFree);
  
  // test the method toMinutes in the class ClockTime
  boolean testToMinutes(Tester t){
    return
    t.checkExpect(this.tenAM.toMinutes(), 600) &&
    t.checkExpect(this.eleven30.toMinutes(), 690);    
  }  
  
  // test the method duration in the class Event
  boolean testDuration(Tester t){
    return
    t.checkExpect(this.fundies2.duration(), 90);    
  }  
  
  // test the method scheduledTime in the union of classes ISchedule
  boolean testScheduledTime(Tester t){
    return
    t.checkExpect(this.allFree.scheduledTime(), 0) &&
    t.checkExpect(this.onlyFundies2.scheduledTime(), 90);   
  }
  
}

/*
              +-----------+
              | ISchedule |<---------------+
              +-----------+                |
                   / \                     |
                   ---                     |
                    |                      |
       -------------------------           |
       |                        |          |
 +----------+          +----------------+  | 
 | NoEvents |          | ConsEvents     |  |
 +----------+          +----------------+  | 
 +----------+       +--| Event first    |  |
                    |  | ISchedule rest |--+
                    |  +----------------+
                    v
       +---------------------+
       | Event               |
       +---------------------+
       | String name         |
       | ClockTime startTime |----+
       | ClockTime endTime   |--+ |
       +---------------------+  | |
                                | |
                                v v
                         +-------------+
                         | ClockTime   |
                         +-------------+
                         | int hours   |
                         | int minutes |
                         +-------------+
*/
