import tester.*;

class Author {
    String name;
    LoB bs;
    Author(String name) {
	this.name = name;
	this.bs = new MTB();
    }

    // EFFECT: add given book to this author's list of books.
    void addBook(Book b) {
	this.bs = new ConsB(b, this.bs);
    }
}

class Book {
    String title;
    LoA as;
    Book(String title, LoA as) {
	this.title = title;
	this.as = as;
	as.addBook(this);
    }
}

interface LoB {}

class MTB implements LoB {
    MTB(){}
}

class ConsB implements LoB {
    Book b;
    LoB rest;
    ConsB(Book b, LoB rest) {
	this.b = b;
	this.rest = rest;
    }
}

interface LoA {
    // EFFECT: add given book to these authors' books.
    void addBook(Book b);
}

class MTA implements LoA {
    MTA(){}

    // EFFECT: add given book to these authors' books.
    public void addBook(Book b) {}
}

class ConsA implements LoA {
    Author a;
    LoA rest;
    ConsA(Author a, LoA rest) {
	this.a = a;
	this.rest = rest;
    }

    // EFFECT: add given book to these authors' books.
    public void addBook(Book b) {
	this.a.addBook(b);
	this.rest.addBook(b);
    }
}


class Examples {

    Author mf = new Author("Matthias");
    Author vkp = new Author("Viera");

    Book htdc = new Book("How to Design Classes",
			 new ConsA(mf, new ConsA(vkp, new MTA())));

    Book htdp = new Book("How to Design Programs",
			 new ConsA(mf, new MTA()));

    void testBook(Tester t) {
	t.checkExpect(mf.bs, new ConsB(htdp,
				       new ConsB(htdc,
						 new MTB())));
	t.checkExpect(htdc.as, new ConsA(mf, new ConsA(vkp, new MTA())));
	t.checkExpect(vkp.bs, new ConsB(htdc, new MTB()));
    }
}
