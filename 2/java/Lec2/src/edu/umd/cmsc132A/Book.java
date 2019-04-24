package edu.umd.cmsc132A;

class Book {
    String title;
    Lo<Author> auths;

    Book(String title, Lo<Author> auths) {
        this.title = title;
        this.auths = auths;
        // auth.book = this;
        // for each a in auths:    a.books = new Cons<>(this, a.books);
        auths.foreach(a -> a.books = new Cons<>(this, a.books));
    }
}
