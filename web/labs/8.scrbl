#lang scribble/manual

@(require "../lab.rkt"
          "../unnumbered.rkt"
          "../utils.rkt")

@(define exercise (exercise-counter))

@title[#:tag "lab8"]{2/27: Java, Dictionaries, and the Internet}

@itemlist[
  @item{Download @tt{prima-1.2.3.jar} from
        @url{http://code.google.com/p/nutester}.}
]

@lab:section{Warmup: same data, different language}

In this lab, we'll be introducing you to designing interface, data,
and class definitions in Java. If you follow the design recipe, the
process is mostly the same, though you may have to think about types
and Java syntax.

Think back to lab 5, in which we built several implementations of the
Dictionary interface. In case you don't remember, here's the interface
again, but in Java:

@indented{@verbatim|{
  // A Key is a String
  //
  // A Dict<V> implements
  //
  // has-key : Key -> Boolean
  // Does the given key exist?
  //
  // lookup : Key -> V
  // The value mapped to by the given key
  //
  // set : Key V -> Dict<V>
  // Set the given key-value mapping
}|}

Note that we have removed the assumption for lookup that the key is
in the dictionary. It turns out that, unlike in @racket[class/0],
Java forces us to implement @racket[lookup] for all classes that implement
the @racket[Dict] interface.

Instead of leaving the method unimplemented, you can just define
the method to return an error when there is no key. In Java, you
can return an error with a statement like this:
@verbatim|{throw new RuntimeException("Error message");}|

@exercise{
  Implement a @racket[Dict] in Java using any of the designs from lab 5.
  Make sure to translate the data definition to the Java syntax first.

  You may find the @racket[compareTo] method on Strings useful for this exercise.
  The @racket[compareTo] method takes an object that implements the @racket[Comparable]
  interface (many built-in Java objects) and returns an @racket[int]. This number
  is negative, zero, and postive respectively when the given object is less than,
  equal to, or greater than @racket[this].
}

There are several other operations that you might want in a
good library for dictionaries. For example, you may want to be able
to extract just the keys or values contained in a dictionary:

@indented{@verbatim|{
  // keys : -> List<Key>
  // return the keys in the dictionary
  //
  // values : -> List<V>
  // return the values in the dictionary
}|}

@tt{List<X>} above is an interface for a generic kind of list. Since
Java has support for generic types, let's take advantage of that and
write down one data and class definition we can use for the results of
both the @tt{keys} and @tt{values} methods.

@indented{@verbatim|{
  // A List<X> is one of:
  //  - new Empty<X>()
  //  - new Cons<X>(X, List<X>)
  //
  // and implements:
  //
  // length : -> Integer
  // The length of the list
  //
  // first : -> X
  // The first element in the list (assumes list is non-empty)
  //
  // rest : -> List<X>
  // The rest of the list (assumes list is non-empty)
}|}

@exercise{
  Define the @racket[List<X>] interface and the @racket[Empty<X>] and
  @racket[Cons<X>] classes that implement it.
}

Now using the class definitions for lists, let's flesh out our dictionaries.

@exercise{
  Implement the @tt{keys} and @tt{values} methods above.
}

Another thing you might want to do is run a function to update the value mapped
by a particular key (instead of just providing the new value). Since Java
doesn’t have higher-order functions, we will simulate them with objects. Here’s
an interface defintion for a unary function from X to Y:

@indented{@verbatim|{
  // A Fun<X,Y> implements:
  //
  // call : X -> Y
  // Call this function on its X input and produce a Y
}|}

Let's use this to define a new method for dictionaries.

@indented{@verbatim|{
  // Dict<V> also implements:
  //
  // update : Key Fun<V, V> -> Dict<V>
  // run the given function to get a new value for key
}|}

@exercise{
  Implement the @tt{update} method.
}

One problem with the data definition above is that it only lets you
use keys that are @tt{Nat}s. Since the only operation that we need
for the key is that it lets us check if two keys are equal, let's
build that into our data definitions:

@indented{@verbatim|{
  // An Eq implements
  //
  // equals : Eq -> Boolean
}|}

We chose the name @tt{equals} because all of the built-in classes
like @tt{Integer} and @tt{String} in Java implements it.

@indented{@verbatim|{
  // A EqDict<V> implements
  //
  // has-key : Eq -> Boolean
  // Does the given key exist?
  //
  // lookup : Eq -> V
  // The value mapped to by the given key
  // Assumption: the key exists
  //
  // set : Eq V -> Dict<V>
  // Set the given key-value mapping
}|}

@exercise{
  Create a new dictionary that uses this new data definition.
}

@lab:section{Another kind of dictionary}

We've seen various implementations of dictionaries, but are they good for
representing real dictionaries like the Merriam-Webster Dictionary that store
words? You might think that since these dictionaries are specialized to
work with words, you could design a faster data structure for them.

Well, it turns out you can. We will design a data structure called a "trie"
(pronounced "tree" or "try") that is highly efficient for storing words.
Here is the data definition for a trie:

@indented{@verbatim|{
  // A Trie<V> is one of
  //  - new NoValue(Dict<Trie<V>>)
  //  - new Node(V, Dict<Trie<V>>)
  // and implements Dict<V>
  //
  // where the keys in the Dict are all single-character strings
}|}

As you can see from the data definition, each trie node stores a dictionary of
its child nodes keyed by single characters.  Imagine each connection from a
node to its child as representing one character in a string. When you lookup a
string in a trie, you look at the first character in the string and continue to
lookup in the corresponding sub-trie. The path you take down the tree will
spell out the string you are looking up.

The advantage of this representation is that the time it takes to lookup
any string is proportional to the length of the string. This is faster than
a binary search tree in general.

@exercise{
  Implement @tt{has-key}, @tt{lookup}, and @tt{set} for tries.
}

One of the operations that a trie makes very fast is searching for all key/value
pairs that matches a certain prefix of a key (a prefix is just a substring of
a string that includes the beginning). This kind of operation is useful if you
want to search through a dictionary.

Here is a contract for a function that search the dictionary based on a prefix:

@indented{@verbatim|{
  // matchPrefix : String -> ListOf<String>
  // Finds all keys that match the given prefix
}|}

@exercise{
  Implement @tt{matchPrefix}.
}
