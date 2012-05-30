import tester.*;

class Person {
    String name;
    Bank acct;
    Person(String name, Bank acct) {
	this.name = name;
	this.acct = acct;
    }

    // EFFECT: this persons bank accout is updated by given amount.
    void paycheck(Integer amt) {
	this.acct.deposit(amt);
	// this.name = "Fred";
    }
}

class Bank {
    Integer dollars;
    Bank(Integer dollars) {
	this.dollars = dollars;
    }

    // EFFECT: update this account balance by given amount
    void deposit(Integer amt) {
	this.dollars = this.dollars + amt;
    }

    // Produce the balance in this account.
    Integer balance() {
	return this.dollars;
    }
}

class Examples {

    void testBalance(Tester t) {
	Bank dvhAcct = new Bank(19);
	t.checkExpect(dvhAcct.balance(), 19);
    }

    void testDeposit(Tester t) {
	Bank dvhAcct = new Bank(19);
	dvhAcct.deposit(250);
	t.checkExpect(dvhAcct, new Bank(269));
    }

    void testPaycheck(Tester t) {
	Bank acct = new Bank(19);
	Bank notOtherAcct = acct;
	Bank otherAcct = new Bank(19);

	Person dvh = new Person("David", acct);
	Person sweetie = new Person("Significant Other",
				    acct);

	dvh.paycheck(250);
	t.checkExpect(dvh, new Person("David", new Bank(269)));
	t.checkExpect(sweetie, new Person("Significant Other", new Bank(269)));

	t.checkOneOf(notOtherAcct, new Bank(269));
	t.checkOneOf(otherAcct, new Bank(19));

	// List<Bank> bank = new Cons<Bank>(sweetie, new Empty<Bank>());
	// bank.interest();
    }

}

class Book {
    String title;
    Author author;
    Book(String title, Author author) {
	this.title = title;
	this.author = author;
	author.writeBook(this);
    }
}

class Author {
    String name;
    Book book;
    Author(String name) {
	this.name = name;
    }
    Author(String name, Book b) {
	this.name = name;
	this.book = b;
    }

    void writeBook(Book book) {
	this.book = book;
    }
}

// HtDC, Felleisen

class ExamplesBooks {
    Book HtDC = new Book("How to Design Classes",
			 new Author("MF"));

    void testHtDC(Tester t) {
	t.checkExpect(HtDC.author, new Author("MF", HtDC));
	t.checkExpect(HtDC.author.book.author.book.author,
		      new Author("MF", HtDC));
    }
}



