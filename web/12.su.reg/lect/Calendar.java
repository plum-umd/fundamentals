import tester.*;

// to represent a time of day in hours and minutes
class ClockTime {
    Integer minutes;  // must be in the range [0, 24)
    Integer hours;    // must be in the range [0, 60)
	
    ClockTime(Integer hours, Integer minutes) {
	this.hours = hours;
	this.minutes = minutes;
    }
    
    /* TEMPLATE
     * FIELDS:
     * ... this.hours ...         -- Integer
     * ... this.minutes ...       -- Integer
     * METHODS:
     * ... this.toMinutes() ...   -- Integer
     * ... this.isBefore() ...    -- Boolean
     * ...
     */
    
    // convert the time on this clock to total minutes since midnight
    public Integer toMinutes() {
	return this.minutes + (60 * this.hours);
    }
    
    // Is this time before that time?
    public Boolean isBefore(ClockTime that) {
	return this.toMinutes() <= that.toMinutes();
    }
}

// to represent an event in a calendar
class Event {
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
     * ... this.duration() ...              -- Integer
     * ... this.endsBefore(Event) ...       -- Boolean
     * ...
     * METHODS FOR FIELDS:
     * ... this.startTime.toMinutes() ...   -- Integer
     * ... this.endTime.toMinutes() ...     -- Integer
     * ... this.startTime.isBefore(ClockTime) ...    -- Boolean
     * ... this.endTime.isBefore(ClockTime) ...    -- Boolean
     */	
    
    // compute the duration of this event in minutes
    public Integer duration() {
	return this.endTime.toMinutes() - this.startTime.toMinutes();
    }

    // Does this event end before that event starts?
    public Boolean endsBefore(Event that) {
	return (this.startTime.toMinutes() + this.duration())
	    <= that.startTime.toMinutes();
    }

    // Does this event overlap with that event?
    public Boolean overlapHuh(Event that) {
	return !(this.endsBefore(that) || that.endsBefore(this));
    }
    
}

// to represent events scheduled for one day
interface ISchedule {
    
    // compute the total scheduled time for this day
    public Integer scheduledTime();
    
    // Is this schedule ordered and non-overlapping?
    public Boolean goodScheduleHuh();

    // Does the first event (if any) in this schedule occur after the given event?
    public Boolean occursAfter(Event e);

    // Compute the schedule of free time in this schedule
    public ISchedule freeTime();
}

class NoEvents implements ISchedule {
    NoEvents() {}
    
    // compute the total scheduled time for this day
    public Integer scheduledTime(){
	return 0;
    }

    // Is this no event schedule ordered and non-overlapping?
    public Boolean goodScheduleHuh() {
	return true;
    }

    // Does the first event (if any) in this schedule occur after the given event?
    public Boolean occursAfter(Event e) {
	return true;
    }

    // Compute the schedule of free time in this schedule
    public ISchedule freeTime() {
	return new ConsEvents(new Event("Free", 
					new ClockTime(0,0), 
					new ClockTime(24,0)),
			      this);
    }

}

class ConsEvents implements ISchedule {
    Event first;
    ISchedule rest;
    
    ConsEvents(Event first, ISchedule rest) {
	this.first = first;
	this.rest = rest;
    }
    
    /* TEMPLATE
     * FIELDS:
     * ... this.first ...                   -- Event
     * ... this.rest ...                    -- ISchedule
     * METHODS:
     * ... this.scheduledTime() ...         -- Integer
     * ...
     * METHODS FOR FIELDS:
     * ... this.first.duration() ...        -- Integer
     * ... this.first.endsBefore(Event) ... -- Boolean
     * ... this.rest.scheduledTime() ...    -- Integer
     */ 
    
    // compute the total scheduled time for this day
    public Integer scheduledTime() {
	return this.first.duration() + this.rest.scheduledTime();
    }

    // Is this cons event schedule ordered and non-overlapping?
    public Boolean goodScheduleHuh() {
	return this.rest.goodScheduleHuh() 
	    && this.rest.occursAfter(this.first);
    }

    // Does the first event in this schedule occur before the given event?
    public Boolean occursAfter(Event e) {
	return e.endsBefore(this.first);
    }

    // Compute the schedule of free time in this cons schedule
    public ISchedule freeTime() {
	return this.freeTimeFrom(new ClockTime(0,0));
    }

    // Compute the schedule of free time, starting from given time.
    public ISchedule freeTimeFrom(ClockTime start) {
	return new ConsEvents(new Event(start, this.first.startTime),
			      this.rest.freeTimeFrom(this.first.endTime));
    }
	


}

class ExamplesCalendar {
    ExamplesCalendar() {}
    
    ClockTime eightFifteen = new ClockTime(8, 15);
    ClockTime nine = new ClockTime(9, 0);
    ClockTime tenAM = new ClockTime(10, 0);
    ClockTime eleven30 = new ClockTime(11, 30);
    ClockTime noon = new ClockTime(12, 0);
    ClockTime one = new ClockTime(13, 0);
    
    Event breakfast = new Event("Breakfast", this.eightFifteen, this.nine);
    Event fundies2 = new Event("Fundies 2", this.tenAM, this.eleven30);
    Event lunch = new Event("Lunch", this.eleven30, this.noon);
    Event logicAndComp = new Event("L&C", this.tenAM, this.noon);
    
    ISchedule allFree = new NoEvents();
    ISchedule onlyFundies2 = new ConsEvents(this.fundies2, this.allFree);
    ISchedule onlyLunch = new ConsEvents(this.lunch, this.allFree);
    
    ISchedule weirdSchedule = new ConsEvents(this.fundies2,
					     new ConsEvents(this.breakfast,
							    this.allFree));

    ISchedule freshmen = new ConsEvents(this.fundies2,
					new ConsEvents(this.logicAndComp,
						       this.allFree));

    // test the method toMinutes in the class ClockTime
    Boolean testToMinutes(Tester t) {
	return t.checkExpect(this.tenAM.toMinutes(), 600) 
	    && t.checkExpect(this.eleven30.toMinutes(), 690);    
    }  
    
    // test the method duration in the class Event
    Boolean testDuration(Tester t) {
	return t.checkExpect(this.fundies2.duration(), 90);    
    }  
    
    // test the method scheduledTime in the union of classes ISchedule
    Boolean testScheduledTime(Tester t) {
	return t.checkExpect(this.allFree.scheduledTime(), 0) 
	    && t.checkExpect(this.onlyFundies2.scheduledTime(), 90);   
    }   

    // test the isBefore method in the class ClockTime
    Boolean testIsBefore(Tester t) {
	return t.checkExpect(this.tenAM.isBefore(this.eleven30), true)
	    && t.checkExpect(this.tenAM.isBefore(this.tenAM), true)
	    && t.checkExpect(this.eleven30.isBefore(this.tenAM), false);
    }
    
    // test the endsBefore method in the class Event
    Boolean testEndsBefore(Tester t) {
	return t.checkExpect(this.fundies2.endsBefore(this.fundies2), false)
	    && t.checkExpect(this.fundies2.endsBefore(this.lunch), true);
    }

    // test the overlapHuh method in the class Event
    Boolean testOverlapHuh(Tester t) {
	return t.checkExpect(this.fundies2.overlapHuh(this.fundies2), true)
	    && t.checkExpect(this.fundies2.overlapHuh(this.lunch), false);
    }

    // test the occursAfter method of ISchedule
    Boolean testOccursAfter(Tester t) {
	return t.checkExpect(this.allFree.occursAfter(this.lunch), true)
	    && t.checkExpect(this.onlyFundies2.occursAfter(this.lunch), false)
	    && t.checkExpect(this.onlyLunch.occursAfter(this.fundies2), true);
    }

    // test the goodScheduleHuh method of ISchedule
    Boolean testGoodSchedule(Tester t) {
	return t.checkExpect(this.allFree.goodScheduleHuh(), true)
	    && t.checkExpect(this.onlyFundies2.goodScheduleHuh(), true)
	    && t.checkExpect(this.weirdSchedule.goodScheduleHuh(), false)
	    && t.checkExpect(this.freshmen.goodScheduleHuh(), false);
    }
    
}
