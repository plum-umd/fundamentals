import tester.*;

class Book {
    String title;
    Authors authors;
    Book(String title, Authors authors) {
	this.title = title;
	this.authors = authors;
	authors.addBook(this);
    }
}

class Author {
    String name;
    Books books;
    Author(String name) {
	this.name = name;
	this.books = new EmptyBooks();
    }
}

interface Authors {
    // EFFECT: Add given book to this list of authors
    void addBook(Book b);
}

class EmptyAuthors implements Authors {

    public void addBook(Book b) {}
}
class ConsAuthors implements Authors {
    Author first;
    Authors rest;
    ConsAuthors(Author first, Authors rest) {
	this.first = first;
	this.rest = rest;
    }

    public void addBook(Book b) {
	// EFFECT: add b to their list of books
	this.first.books = new ConsBooks(b, this.first.books);
	this.rest.addBook(b);
    }
}

interface Books {}
class EmptyBooks implements Books {
}

class ConsBooks implements Books {
    Book first;
    Books rest;
    ConsBooks(Book first, Books rest) {
	this.first = first;
	this.rest = rest;
    }
}

class Examples {

    void testAlsoThis(Tester t) {
	Author vkp = new Author("VKP");
	Author mf = new Author("MF");
	Book htdc = new Book("HtDC", new ConsAuthors(mf, new ConsAuthors(vkp, new EmptyAuthors())));
	Book tls = new Book("The Little Schemer", new ConsAuthors(mf, new EmptyAuthors()));
	t.checkExpect(1,1);
    }

    void testBooks(Tester t) {
	Author vkp = new Author("VKP");
	Author mf = new Author("MF");
	Book htdc = new Book("HtDC", new ConsAuthors(mf, new ConsAuthors(vkp, new EmptyAuthors())));
	t.checkExpect(vkp.books, new ConsBooks(htdc, new EmptyBooks()));
	t.checkExpect(mf.books, new ConsBooks(htdc, new EmptyBooks()));
	Book htdp = new Book("HtDP", new ConsAuthors(mf, new EmptyAuthors()));
	t.checkExpect(mf.books, new ConsBooks(htdp, new ConsBooks(htdc, new EmptyBooks())));
    }
}

