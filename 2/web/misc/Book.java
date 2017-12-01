import tester.*;

// Author  is new Author(String name, List<Books> books)
// Book is new Book(String title, Author author)
class Author {
    String name;
    List<Book> books;
    Author(String name, List<Book> books) {
	this.name = name;
	this.books = books;
    }

    Author(String name) {
	this.name = name;
	this.books = new Mt<Book>();
    }

    // add this book to the list of books by this author
    // Effect: changes this list to have one more element 
    void addBook(Book b) {
	this.books = new Cons<Book>(b, this.books);
    }

}

// Book:

class Book {
    String title;
    List<Author> authors;
    
    Book(String t, List<Author> a) {
	// part 1
	this.title = t;
	this.authors = a;
	// part 2
	a.map(new BookAdder(this));
    }
}

class BookAdder implements Function<Author,Author> {
    Book b;
    BookAdder(Book b) { this.b = b; }

    public Author call(Author a) {
	a.addBook(this.b);
	return a;
    }

}

class Examples {
    boolean test_one (Tester t) {
	List<Author> sm = new Cons<Author>(new Author("Stephanie Meyer"), new Mt<Author>());
	Book b = new Book("Twilight", sm);
	Book b2 = new Book("New Moon", sm);

	return t.checkExpect(b.authors, b2.authors)
	    && t.checkExpect (b.authors.len (), 1)
	    && t.checkExpect (b2.authors.len (), 1);
    }
}


interface List<T> {
    Integer len();
    <U>  List<U> map(Function<T,U> f);
}

interface Function<T,U> {
    U call (T v);
}

class Mt<T> implements List<T> {
    public Integer len() { return 0; }
    public <U>  List<U> map(Function<T,U> f) {
	return new Mt<U>();
    }
	
}
class Cons<T> implements List<T> {
    T f;
    List<T> r;
    Cons(T f, List<T> r) {
	this.f = f; this.r = r;
    }
    public <U>  List<U> map(Function<T,U> f) {
	return new Cons<U>(f.call(this.f), this.r.map(f));
    }
    public Integer len() { return 1 + this.r.len(); }
}