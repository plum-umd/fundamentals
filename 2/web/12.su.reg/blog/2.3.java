import tester.*;

// Represents a life (as a tree of choices)
interface Life {
    // Minimum number of choices before death in this life.
    Integer minChoices();

    // Maximum number of choices before death in this life.
    Integer maxChoices();

    // Number of choices if always answer "yes" in this life.
    Integer yesChoices();

    // Number of choices if always answer "no" in this life.
    Integer noChoices();

    // Number of choices if always answer "yes/no" starting with yes.
    Integer altChoicesYes();

    // Number of choices if always answer "yes/no" starting with no.
    Integer altChoicesNo();

    // List of questions in this life if always answer "yes".
    LoQ yesQuestions();

    // List of questions in this life if always answer "yes/no" starting w/yes.
    LoQ altQuestionsYes();

    // List of questions in this life if always answer "yes/no" starting w/no.
    LoQ altQuestionsNo();
}

// Represents the end of life
class Die implements Life {
    String reason; // Optional
    Die(String reason) {
	this.reason = reason;
    }

    // Minimum number of choices before death in this end of life.
    public Integer minChoices() {
	return 0;
    }

    // Maximum number of choices before death in this end of life.
    public Integer maxChoices() {
	return 0;
    }

    // Number of choices if always answer "yes" in this end of life.
    public Integer yesChoices() {
	return 0;
    }

    // Number of choices if always answer "no" in this end of life.
    public Integer noChoices() {
	return 0;
    }

    // Number of choices if always answer "yes/no" starting with yes.
    public Integer altChoicesYes() {
	return 0;
    }

    // Number of choices if always answer "yes/no" starting with no.
    public Integer altChoicesNo() {
	return 0;
    }

    // List of questions in this life if always answer "yes".
    public LoQ yesQuestions() {
	return new MTQ();
    }

    // List of questions in this life if always answer "yes/no" starting w/yes.
    public LoQ altQuestionsYes() {
	return new MTQ();
    }

    // List of questions in this life if always answer "yes/no" starting w/no.
    public LoQ altQuestionsNo() {
	return new MTQ();
    }
}

// Represents a choice in life
class Choice implements Life {
    String desc;
    Life yes;
    Life no;
    Choice(String desc, Life yes, Life no) {
	this.desc = desc;
	this.yes = yes;
	this.no = no;
    }

    // Minimum number of choices before death in this choice of life.
    public Integer minChoices() {
	return 1 + Math.min(this.yes.minChoices(),
			    this.no.minChoices());
    }

    // Maximum number of choices before death in this choice of life.
    public Integer maxChoices() {
	return 1 + Math.max(this.yes.maxChoices(),
			    this.no.maxChoices());
    }

    // Number of choices if always answer "yes" in this choice of life.
    public Integer yesChoices() {
	return 1 + this.yes.yesChoices();
    }

    // Number of choices if always answer "no" in this choice of life.
    public Integer noChoices() {
	return 1 + this.no.noChoices();
    }

    // Number of choices if always answer "yes/no" starting with yes.
    public Integer altChoicesYes() {
	return 1 + this.yes.altChoicesNo();
    }

    // Number of choices if always answer "yes/no" starting with no.
    public Integer altChoicesNo() {
	return 1 + this.no.altChoicesYes();
    }

    // List of questions in this life if always answer "yes".
    public LoQ yesQuestions() {
	return new ConsQ(this.desc, this.yes.yesQuestions());
    }

    // List of questions in this life if always answer "yes/no" starting w/yes.
    public LoQ altQuestionsYes() {
	return new ConsQ(this.desc, this.yes.altQuestionsNo());
    }

    // List of questions in this life if always answer "yes/no" starting w/no.
    public LoQ altQuestionsNo() {
	return new ConsQ(this.desc, this.no.altQuestionsYes());
    }
}


// Represents a list of questions
interface LoQ {}
class MTQ implements LoQ {}
class ConsQ implements LoQ {
    String first;
    LoQ rest;
    ConsQ(String first, LoQ rest) {
	this.first = first;
	this.rest = rest;
    }
}

class Examples {
    Examples() {}

    Life damned = new Die("Die!");
    Life dodont = new Choice("Damned if you do, damned if you don't.",
			     damned, damned);
    Life dbldamn = new Choice("Double damned",
			      dodont,
			      dodont);
    Life real = new Choice("Born?",
			   new Choice("Taxes?",
				      new Choice("Old?",
						 new Die("The Bad"),
						 new Die("The Good")),
				      new Die("Death by state")),
			   new Die("Death at birth"));

    void testMinChoices(Tester t) {
	t.checkExpect(damned.minChoices(), 0);
	t.checkExpect(dodont.minChoices(), 1);
	t.checkExpect(real.minChoices(), 1);
    }

    void testMaxChoices(Tester t) {
	t.checkExpect(damned.maxChoices(), 0);
	t.checkExpect(dodont.maxChoices(), 1);
	t.checkExpect(real.maxChoices(), 3);
    }

    void testYesChoices(Tester t) {
	t.checkExpect(damned.yesChoices(), 0);
	t.checkExpect(dodont.yesChoices(), 1);
	t.checkExpect(real.yesChoices(), 3);
    }

    void testNoChoices(Tester t) {
	t.checkExpect(damned.noChoices(), 0);
	t.checkExpect(dodont.noChoices(), 1);
	t.checkExpect(real.noChoices(), 1);
    }

    void testAltChoices(Tester t) {
	t.checkExpect(damned.altChoicesYes(), 0);
	t.checkExpect(dodont.altChoicesYes(), 1);
	t.checkExpect(real.altChoicesYes(), 2);
	t.checkExpect(damned.altChoicesNo(), 0);
	t.checkExpect(dodont.altChoicesNo(), 1);
	t.checkExpect(real.altChoicesNo(), 1);
    }

    void testQuestions(Tester t) {
	t.checkExpect(damned.yesQuestions(), new MTQ());
	t.checkExpect(dodont.yesQuestions(),
		      new ConsQ("Damned if you do, damned if you don't.",
				new MTQ()));
	t.checkExpect(damned.altQuestionsYes(), new MTQ());
	t.checkExpect(real.altQuestionsYes(),
		      new ConsQ("Born?", new ConsQ("Taxes?", new MTQ())));
    }
}
