import tester.*;

// Interface for two kinds of functional objects:
// - predicates on runners
// - comparison between runners

// Represents a predicate on Runners.
interface RunnerQuestion {
    // Ask this question of the runner.
    Boolean ask(Runner r);
}

// Represent a comparison between two Runners.
interface RunnerComparison {
    // Is r1 "better" than r2?
    Boolean compare(Runner r1, Runner r2);
}

// Represents the comparison: is r1 faster than r2?
class IsFaster implements RunnerComparison {
    // Is r1 faster than r2?
    public Boolean compare(Runner r1, Runner r2) {
	return r1.time < r2.time;
    }
}

class HasLongerName implements RunnerComparison {
    // Does r1 have a longer name than r2?
    public Boolean compare(Runner r1, Runner r2) {
	return r1.name.length() > r2.name.length();
    }
}

class HasLowerBibNumber implements RunnerComparison {
    // Does r1 have a lower bib number than r2?
    public Boolean compare(Runner r1, Runner r2) {
	return r1.bib < r2.bib;
    }
}

class Lexicographic implements RunnerComparison {
    // Does r1's name come before r2's, alphabetically?
    public Boolean compare(Runner r1, Runner r2) {
	return r1.name.compareTo(r2.name) < 0;
    }
}

// Represents the predicate: is given runner older than 50?
class IsOld implements RunnerQuestion {
    Integer age;

    // By default, old is > 50 years.
    IsOld() {
	this(50);
    }

    IsOld(Integer age) {
	this.age = age;
    }

    // Is the runner's age greater than this's age?
    public Boolean ask(Runner r) {
	return r.age > this.age;
    }
}

// Represents the predicate: is given runner's time 130 or better?
class IsFast implements RunnerQuestion {
    public Boolean ask(Runner r) {
	return r.time <= 130;
    }
}

// Represents the predicate: is given runner named "Shorter"?
class IsNamedShorter implements RunnerQuestion {
    public Boolean ask(Runner r) {
	return r.name.equals("Shorter");
    }
}

// Represents the predicate: if given runner is old, are they fast?
class IsOldFast implements RunnerQuestion {
    public Boolean ask(Runner r) {
	return r.age <= 50 || r.time <= 200;
    }
}

// Represents non-empty lists of runners
interface IConsLoR extends ILoR {
    // Find the runner with fastest time in this non-empty list.
    Runner winner();

    // Find the best runner according to given comparison in this
    // non-empty list.
    Runner best(RunnerComparison c);
}

// Represents a list of runners
interface ILoR {    
    // Are all of the runners in this list over 50?
    Boolean allOld();
    
    // Are all of the runners' time in this list 130 or better?
    Boolean allFast();	

    // Do all the runners older than 50 have times less than 200 in this list?
    Boolean allOldFast();

    // Is the question true for all the runners in this list?
    Boolean all(RunnerQuestion q);

    // Is the question true of any of the runners in this list?
    Boolean any(RunnerQuestion q);

    // Produce this list of runner sorted in ascending order by time.
    ILoR sort();

    // Produce this list of runners sorted according to the given comparison.
    ILoR sort(RunnerComparison c);

    // Insert runner into this sorted list according to given comparison.
    ILoR insert(Runner r, RunnerComparison c);

    // Produce list of elements in this list for which the given question is true.
    ILoR filter(RunnerQuestion q);

    // ABSTRACTED AWAY
    // Find runner with best time between runners in this list and
    // given runner.
    // Runner winnerAcc(Runner bestSoFar);	   

    // Find the best runner between the runners in this list and the
    // given runner according to the given comparison.
    Runner bestAcc(Runner bestSoFar, RunnerComparison c);	
}

abstract class ALoR implements ILoR {

    // Are all of the runners in this list over 50?
    public Boolean allOld() {
	return this.all(new IsOld());
    }
    
    // Are all of the runners' time in this list 130 or better?
    public Boolean allFast() {
	return this.all(new IsFast());
    }

    // Do all the runners older than 50 have times less than 200 in this list?
    public Boolean allOldFast() {
	return this.all(new IsOldFast());
    }

    // Is the question true for all the runners in this list?
    abstract public Boolean all(RunnerQuestion q);

    abstract public Boolean any(RunnerQuestion q);

    abstract public ILoR sort(RunnerComparison c);

    // By default, sort by ascending time.
    public ILoR sort() {
	return this.sort(new IsFaster());
    }

    // Produce list of elements in this list for which the given question is true.
    abstract public ILoR filter(RunnerQuestion q);

    // ABSTRACTED AWAY
    // abstract public Runner winnerAcc(Runner fastestSoFar);
    
    abstract public Runner bestAcc(Runner bestSoFar, RunnerComparison c);	
}

// The empty list of runners
class MTLoR extends ALoR {    
    // Is the question true for all the runners in this empty list?
    public Boolean all(RunnerQuestion q) {
	return true;
    }

    // Is the question true of any runner in this empty list?
    public Boolean any(RunnerQuestion q) {
	return false;
    }

    // Sort this empty list
    public ILoR sort(RunnerComparison c) {
	return this;
    }

    // Insert runner into empty list
    public ILoR insert(Runner r, RunnerComparison c) {
	return new ConsLoR(r, this);
    }

    // Produce list of elements in this empty list for which the given question is true.
    public ILoR filter(RunnerQuestion q) {
	return this;
    }

    // ABSTRACTED AWAY
    /*
    // Find runner with best time between runners in this empty list
    // and given runner.
    public Runner winnerAcc(Runner fastestSoFar) {
	return fastestSoFar;
    }
    */

    // Find best runner between runners in this empty list and given runner.
    public Runner bestAcc(Runner bestSoFar, RunnerComparison c) {
	return bestSoFar;
    }
}

// A non-empty list of runners
class ConsLoR extends ALoR implements IConsLoR {
    Runner first;
    ILoR rest;
    ConsLoR(Runner first, ILoR rest) {
	this.first = first;
	this.rest = rest;
    }

    // Is the question true for all runners in this non-empty list?
    public Boolean all(RunnerQuestion q) {
	return q.ask(this.first)
	    && this.rest.all(q);
    }

    // Is the question true of any runner in this non-empty list?
    public Boolean any(RunnerQuestion q) {
	return q.ask(this.first)
	    || this.rest.any(q);
    }

    // Insert runner into this non-empty sorted list according to comparison.
    public ILoR insert(Runner r, RunnerComparison c) {
	if (c.compare(r, this.first)) {
	    return new ConsLoR(r, this);
	} else {
	    return new ConsLoR(this.first, this.rest.insert(r, c));
	}
    }

    // Sort this non-empty list according to given comparison.
    public ILoR sort(RunnerComparison c) {
	return this.rest.sort(c).insert(this.first, c);
    }

    /*
    // Find runner with best time between runners in this non-empty
    // list and given runner.
    public Runner winnerAcc(Runner fastestSoFar) {		
	if (this.first.time < fastestSoFar.time) {
	    return this.rest.winnerAcc(this.first);
	} else {
	    return this.rest.winnerAcc(fastestSoFar);
	}       
    }
    */

    // Produce list of elements in this non-empty list for which the
    // given question is true.
    public ILoR filter(RunnerQuestion q) {
	if (q.ask(this.first)) {
	    return new ConsLoR(this.first, this.rest.filter(q));
	} else {
	    return this.rest.filter(q);
	}
    }

    // Find best runner between runners in this non-empty list and
    // given runner according to comparison.
    public Runner bestAcc(Runner bestSoFar, RunnerComparison c) {
	if (c.compare(this.first, bestSoFar)) {
	    return this.rest.bestAcc(this.first, c);
	} else {
	    return this.rest.bestAcc(bestSoFar, c);
	}       
    }
	
    // Find runner with best time in this non-empty list.
    public Runner winner() {
	// ABSTRACTED AWAY
	// return this.rest.winnerAcc(this.first);
	return this.best(new IsFaster());
    }

    // Find the best runner according to given comparison in this
    // non-empty list.
    public Runner best(RunnerComparison c) {
	return this.rest.bestAcc(this.first, c);
    }
	
}

// Represents a runner with name, age (in years), bib number, and time
// (in minutes).
class Runner {
    String name;
    Integer age;
    Integer bib;
    Integer time;
    Runner(String name, Integer age, Integer bib, Integer time) {
	this.name = name;
	this.age = age;
	this.bib = bib;
	this.time = time;
    }
}

class Examples {
    Runner johnny = new Runner("Kelly", 100, 999, 190);
    Runner frank = new Runner("Shorter", 32, 888, 130);
    Runner bill = new Runner("Rogers", 36, 777, 129);
    
    ILoR mt = new MTLoR();
    IConsLoR rs = 
	new ConsLoR(johnny, 
		    new ConsLoR(frank,
				new ConsLoR(bill, mt)));

    ILoR sortedRsAscTime = 
	new ConsLoR(bill,
		    new ConsLoR(frank,
				new ConsLoR(johnny, mt)));

    ILoR sortedRsDscNameLength = 
	new ConsLoR(frank,
		    new ConsLoR(bill,
				new ConsLoR(johnny, mt)));

    // Happy coincidence
    ILoR sortedRsAscBibNumber = sortedRsAscTime;	
    
    ILoR sortedAlpha = 
	new ConsLoR(johnny,
		    new ConsLoR(bill,
				new ConsLoR(frank, mt)));

    ILoR fs = new ConsLoR(frank, mt);
 
    Boolean testQuestions(Tester t) {
	return t.checkExpect(new IsOld().ask(johnny), true)
	    && t.checkExpect(new IsOld().ask(frank), false)
	    && t.checkExpect(new IsNamedShorter().ask(johnny), false)
	    && t.checkExpect(new IsNamedShorter().ask(frank), true)
	    && t.checkExpect(new IsOldFast().ask(johnny), true)
	    && t.checkExpect(new IsOldFast().ask(frank), true);
    }

    Boolean testComparisons(Tester t) {
	return t.checkExpect(new IsFaster().compare(johnny, frank), false)
	    && t.checkExpect(new IsFaster().compare(frank, johnny), true)
	    && t.checkExpect(new HasLongerName().compare(johnny, frank), false)
	    && t.checkExpect(new HasLongerName().compare(frank, johnny), true)
	    && t.checkExpect(new HasLowerBibNumber().compare(johnny, frank), false)
	    && t.checkExpect(new HasLowerBibNumber().compare(frank, johnny), true)
	    && t.checkExpect(new Lexicographic().compare(johnny, frank), true)
	    && t.checkExpect(new Lexicographic().compare(frank, johnny), false);
    }
   
    Boolean testAllOldFast(Tester t) {
	return t.checkExpect(mt.allOldFast(), true)
	    && t.checkExpect(rs.allOldFast(), true);
    }

    Boolean testAllOld(Tester t) {
	return t.checkExpect(mt.allOld(), true)
	    && t.checkExpect(rs.allOld(), false);

    }

    Boolean testAllFrank(Tester t) {
	return t.checkExpect(mt.all(new IsNamedShorter()), true)
	    && t.checkExpect(fs.all(new IsNamedShorter()), true)
	    && t.checkExpect(rs.all(new IsNamedShorter()), false);
    }

    Boolean testAllOlder(Tester t) {
	return t.checkExpect(rs.all(new IsOld(20)), true) 
	    && t.checkExpect(rs.all(new IsOld(150)), false);
    }

    Boolean testAnyOlder(Tester t) {
	return t.checkExpect(rs.any(new IsOld(70)), true);
    }

    Boolean testSort(Tester t) {
	return t.checkExpect(rs.sort(), sortedRsAscTime)
	    && t.checkExpect(rs.sort(new IsFaster()), sortedRsAscTime)
	    && t.checkExpect(rs.sort(new HasLongerName()), sortedRsDscNameLength)
	    && t.checkExpect(rs.sort(new HasLowerBibNumber()), sortedRsAscBibNumber)
	    && t.checkExpect(rs.sort(new Lexicographic()), sortedAlpha);
    }

    Boolean testFilter(Tester t) {
	return t.checkExpect(mt.filter(new IsNamedShorter()), mt)
	    && t.checkExpect(rs.filter(new IsNamedShorter()), fs);
    }

    Boolean testWinner(Tester t) {
	return t.checkExpect(rs.winner(), bill);
    }

    Boolean testBest(Tester t) {
	return t.checkExpect(rs.best(new IsFaster()), bill)
	    && t.checkExpect(rs.best(new HasLongerName()), frank)
	    && t.checkExpect(rs.best(new HasLowerBibNumber()), bill)
	    && t.checkExpect(rs.best(new Lexicographic()), johnny);
    }
}