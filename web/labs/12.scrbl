#lang scribble/manual

@(require "../lab.rkt"
          "../unnumbered.rkt"
          "../utils.rkt")

@(define exercise (exercise-counter))

@title[#:tag "lab12"]{3/26: Hashes and bloom filters}

@lab:section{Warmup with sets}

We know you're sick of dictionaries, so we're going to talk about a
new kind of data structure that's not a dictionary. We're going to talk about
sets.

Many of you are taking a discrete mathematics course, so you probably have
an idea of how pervasive sets are in computer science. Many algorithms depend
on having a set data structure around. They provide an interface like this:

@indented{@verbatim|{
  // A Set<T> implements
  //
  // add : T -> Void
  // Adds an element to the set
  // Effect: updates the set
  //
  // contains : T -> Boolean
  // Check if an element is in the set
}|}

The key thing about sets is they let you check if an element is in it.
That matches up with how sets are defined in mathematics.

Sets are important enough that Java comes with a
@hyperlink["http://docs.oracle.com/javase/1.5.0/docs/api/java/util/Set.html"
           @racket[Set]]
interface.

Before we move on, can you implement a @racket[Set] with any of the
data structures you have implemented so far using delegation?

@exercise{
  Optional:
  Design a data definition and class definition that implements the
  @racket[Set] interface. You should delegate to a data structure you
  have already designed in a previous lab or assignment.
  (e.g., lists, dictionaries, etc.)

  NB: If you feel that you know this material well enough already or
  if it is taking too much time, move on to the next non-optional exercise.
}

It would also be handy to be able to iterate over the elements of a set.

@exercise{
  Optional:
  Update your @racket[Set] class to implement @racket[Iterable].
}

@lab:section{Bloom filters}

Since in class we have been discussing hash tables and hash codes, we will
look at a hash-based implementation of the @racket[Set] interface.
@hyperlink["http://en.wikipedia.org/wiki/Bloom_filter"]{Bloom filter}s
are an efficient way to represent sets that are @emph{probabilistic}.
That is, sometimes it will lie and tell you that an element is in the
set when it is not. However, it will never lie that an element is @emph{not}
in the set.

The way that you implement a bloom filter is by using a @emph{bit vector}.
A bit vector is like a @racket[List] where each position represents a single bit
(true or false, 1 or 0, etc.) and you can either get, set, or flip bits in
your vector. It implements this interface:

@indented{@verbatim|{
  // A BitVector is a
  //   new BitVector(Integer size)
  // and implements
  //
  // get : Integer -> Boolean
  // Produce the bit at the given position
  //
  // set : Integer -> Void
  // Set the bit to true at the given position
  // Effect: updates the vector
  //
  // flip : Integer -> Void
  // Flip a bit at the position
  // Effect: updates the vector
}|}

@exercise{
  Implement a bit vector.
}

As an aside, here is an application of bit vectors: a neat algorithm for
sorting that uses bit vectors.  Suppose you're sorting several million distinct
integers and you know the maximum size of any given integer you're sorting.
You may not want to use a typical sorting algorithm because it will
allocate a bunch of intermediate data structures into your computer's RAM.

Instead, you can build a single bit vector that's as long as the maximum size.
Then, when you see an integer just set the bit with the integer as the
position.  After processing all of your integers, just read off all of the
positions setto true in your bit vectors in order, which gives you the sorted
list. This is guaranteed to work if your integers are all distinct.

@exercise{
  Optional: Implement the bit vector sorting algorithm explained above.

  Test it with a random list of a few million distinct integers.
}

Okay, back to bloom filters. Here is the data definition:

@indented{@verbatim|{
  // A BloomFilter<T> is a
  //   new BloomFilter<T>(Integer k, Integer m)
  //
  // and implements Set<T>
}|}

A bloom filter is built with a bit vector of size @racket[m]. How a
bit vector works is that when you add an element to it, you use
@racket[k] different hash codes to give you @racket[k] positions
to set in your bit vector.

When you do a lookup, you will compute the same @racket[k] hash codes.
If a bit at any position is @emph{not} set, then you know that
the element is definitely not in the set. However, if all bits @emph{are}
set, then you only know that it might be in the set.

The reason you don't know if the element is actually in the set is
because there's a chance another element would have the same positions
because of how you modulo the hash codes. If you increase both @racket[k]
and @racket[m] it is much less likely that you will get these false
positives.

One thing you might be wondering is how you obtain @racket[k] different
hash codes when you only have one @racket[hashCode] method. We can
use a trick with random numbers to do this:

@indented{@verbatim|{
import java.util.List;
import java.util.ArrayList;
import java.util.Random;

public class HashGenerator {
  // hashCode : Object Integer Integer -> List<Integer>
  // produces k hashCodes for the given object
  public static List<Integer> hashCodes(Object o, Integer k, Integer m) {
        Random rand = new Random(o.hashCode());
    ArrayList<Integer> keys = new ArrayList<Integer>();

    return hashCodesAccum(o, k, m, keys, rand);
  }

  // hashCodesAccum : Object Integer Integer List<Integer> -> List<Integer>
  // accumulator helper for the function above
  // Invariant: lst is the list of keys generated so far
  private static List<Integer> hashCodesAccum(Object o, Integer k, Integer m,
                  List<Integer> keys, Random rand) {
    Integer next = rand.nextInt(m);

    if (k == 0) {
      return keys;
    }
    else {
      keys.add(next);
      return hashCodesAccum(o, k - 1, m, keys, rand);
    }
  }
}
}|}

Using the @racket[HashGenerator.hashCodes] method defined above, you can
generate @racket[k] bit vector positions based on the result of any object's
@racket[hashCode] method.

@exercise{
  Using bit vectors and hash codes, implement a @racket[BloomFilter].
}

@exercise{
  Randomly test your @racket[BloomFilter] by adding many (try 10s, then 100s,
of elements) random elements and testing if they're in the set. The elements
can be @racket[Integer]s or anything else you can randomly generate.

  You should notice that for small bloom filters and good values of @racket[k]
and @racket[m] that your bloom filter will rarely lie. Does it keep working
if you increase the number of elements?
}

We should note that the method we used to generate @racket[k] hash codes
above is not very good because it's not guaranteed to give you well
distributed hash codes. It turns out that there are much better hashing
mechanisms such as
@hyperlink["http://en.wikipedia.org/wiki/Murmurhash"]{MurmurHash}.

We have provided an implementation of MurmurHash (based on a public domain
implementation by Derek Young
@hyperlink["http://dmy999.com/article/50/murmurhash-2-java-port"]{here})
which you can get @hyperlink["http://www.ccs.neu.edu/home/asumu/cs2510h/MurmurHash2.java"]{here}.

It implements the same interface as the @racket[HashGenerator] above but
you'll have to invoke @racket[MurmurHash2.hashCodes] instead.

@exercise{
  Use the MurmurHash implementation in your bloom filter. Does it do
  any better than your original for false positives?
}
