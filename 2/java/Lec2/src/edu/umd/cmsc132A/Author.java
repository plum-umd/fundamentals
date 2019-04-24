package edu.umd.cmsc132A;

class Author {
    String name;
    Lo<Book> books;
    Author(String name) {
        this.name = name;
        this.books = new Empty<>();
    }

    Boolean isConnectedAcc(Author a, Lo<Author> seen) {
        return !(seen.ormap(o -> this.equals(o))) &&
                 (this.equals(a) || this.coauthors().ormap(o -> o.isConnectedAcc(a, new Cons<>(this, seen))));
    }

    // Is this author connected to the given author via co-authorship?
    Boolean isConnected(Author a) {
        return this.isConnectedAcc(a, new Empty<>());
    }

    // Produce the list of immediate co-authors of this author
    // May include duplicates
    Lo<Author> coauthors() {
        return this.books.foldr((Book b, Lo<Author> as) -> b.auths.app(as), new Empty<>());
    }
}
