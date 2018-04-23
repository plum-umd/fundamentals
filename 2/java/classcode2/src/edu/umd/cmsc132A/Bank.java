package edu.umd.cmsc132A;

import java.util.Comparator;
import java.util.Optional;
import java.util.function.*;

class Bank {
    Integer amt;

    Bank(Integer amt) {
        this.amt = amt;
    }

    // EFFECT: Deposit the given amount into this account
    void deposit(Integer dep) {
        this.amt = this.amt + dep;
    }

    // EFFECT: Withdraw the given amount from this account
    void withdraw(Integer withd) {
        this.amt = this.amt - withd;
    }
}


class Person implements Comparable<Person> {
    String name;
    Bank b;

    Person(String name, Bank b) {
        this.name = name;
        this.b = b;
    }

    // EFFECT: deposit a paycheck into this person's bank account
    void payday() {
        this.b.deposit(10000);
    }

    public int compareTo(Person p) {
        return this.name.compareTo(p.name);
    }
}


class NameLengthOrder implements Comparator<Person> {
    public int compare(Person p1, Person p2) {
        return p1.name.length() - p2.name.length();
    }
}

class BankAmtOrder implements Comparator<Person> {
    public int compare(Person p1, Person p2) {
        return p1.b.amt - p2.b.amt;
    }
}

class Counter {
    Integer i;

    Counter(Integer i) {
        this.i = i;
    }

    // EFFECT: tick the counter by one
    void tick() {
        this.i = this.i + 1;
    }
}


class BookN {
    String title;
    String ISBN;
    Listof<AuthorN> authors;

    BookN(String title, String ISBN, Listof<AuthorN> authors) {
        this.title = title;
        this.ISBN = ISBN;
        this.authors = authors;
        // update all of the authors to add this book to their list
        // authors.map(a -> { a.addBook(this); return true; });
        authors.forEach(a -> a.addBook(this));
    }
}

class AuthorN {
    String name;
    Listof<BookN> books;

    AuthorN(String name) {
        this.name = name;
        this.books = new Empty<BookN>();
    }

    // EFFECT: updates this author's list of books to include given one
    void addBook(BookN b) {
        this.books = new Cons<>(b, this.books);
    }

    // Produce the list of immediate co-authors of this author
    Listof<AuthorN> coauthors() {
        return this.books.foldr((BookN b, Listof<AuthorN> as) -> b.authors.append(as), new Empty<>());
    }

    // Is the given author connected by co-authorship with this author?
    Boolean isConnected(AuthorN a) {
        return this.isConnectedAcc(a, new Empty<>());
    }

    Boolean isConnectedAcc(AuthorN a, Listof<AuthorN> seen) {
        return !(seen.exists(o -> this.equals(o))) &&
                (this.equals(a) ||
                        this.coauthors().exists(o -> o.isConnectedAcc(a, new Cons<>(this, seen))));
    }

}

class Book {
    String title;
    String ISBN;
    Author author;

    Book(String title, String ISBN, Author author) {
        this.title = title;
        this.ISBN = ISBN;
        this.author = author;
        // effect on author: update their book to this book
        this.author.setBook(this);
    }
}

class Author {
    String name;
    Book book;

    Author(String name) {
        this.name = name;
        this.book = null;
    }

    Author(String name, Book book) {
        this.name = name;
        this.book = book;
    }

    // EFFECT: set this author's book to the given one
    void setBook(Book b) {
        this.book = b;
    }
}


class Name extends Object {
    String first;
    String last;

    Name(String first, String last) {
        this.first = first;
        this.last = last;
    }

    boolean equals(Name n) {
        return this.first.equals(n.first) &&
                this.last.equals(n.last);
    }


    // For every object o, p.   if o.hashCode() != p.hashCode(), then !o.equals(p)
    public int hashCode() {
        return this.first.hashCode();
    }
}

// Comparable<X>
// int compareTo(X x)

// Comparator<X>





























