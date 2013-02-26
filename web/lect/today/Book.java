import tester.*;

/* Cyclic data: this code motivates the use of mutation for creating
   cyclic data. */

// Interp: a single-author book
class Book {
    String title;
    Author author;
    Book(String title, Author author) {
	this.title = title;
	this.author = author;
	author.addBook(this);
    }
}

// Interp: a single-book author
class Author {
    String name;
    Book book;
    Author(String name) {
	this.name = name;
	this.book = null; // Don't touch the void!!
    }

    // EFFECT: set this author's book to given book.
    void addBook(Book book) {
	this.book = book;
    }
}

class Examples {
    void testBooks(Tester t) {
	Author vkp = new Author("VKP");
	Book htdc = new Book("HtDC", vkp);
	t.checkExpect(vkp.book, htdc);
	t.checkExpect(htdc.author, vkp);
	t.checkExpect(vkp.book.author.book.author.book, htdc);
    }
}


