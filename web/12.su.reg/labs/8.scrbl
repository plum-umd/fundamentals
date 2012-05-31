#lang scribble/manual

@(require "../lab.rkt"
          "../unnumbered.rkt"
          "../utils.rkt"
          scribble/eval)

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require lang/htdp-intermediate))
    the-eval))


@(define exercise (exercise-counter))

@title[#:tag "lab08"]{5/31: Changing associations}

The goal of this lab is to practice designing visitors, equality
methods, and mutable structures.

@lab:section{Association lists}

We saw association lists on today's exam, which are used to associated
keys with values.  We can represent associations as follows:

@verbatim|{
interface Assoc<K,V> {}

class EmptyAssoc<K,V> implements Assoc<K,V> {}

class ConsAssoc<K,V> implements Assoc<K,V> {
  K key;
  V val;
  Assoc<K,V> rest;
  ConsAssoc(K key, V val, Assoc<K,V> rest) {
    this.key = key;
    this.val = val;
    this.rest = rest;
  }
}
}|

You wrote a @tt{same} method that determined whether two association
lists were the structurally equal.  But structural equality doesn't
really embody what it means for two association lists to be the same
association.  For example, the following two associations represent
the same information:

@verbatim|{
    Assoc<String,Integer> a1 =
	new ConsAssoc<String,Integer>("Fred", 40,
	    new ConsAssoc<String,Integer>("Wilma", 95,
		new EmptyAssoc<String,Integer>()));

    Assoc<String,Integer> a2 =
	new ConsAssoc<String,Integer>("Wilma", 95,
	    new ConsAssoc<String,Integer>("Fred", 40,
		new EmptyAssoc<String,Integer>()));
}|

Moreover, since we really only pay attention to the first association
of any key in a list, @tt{a1} and @tt{a2} also represent the same
information as this list:

@verbatim|{
    Assoc<String,Integer> a3 =
        new ConsAssoc<String,Integer>("Wilma", 95,
	    new ConsAssoc<String,Integer>("Fred", 40,
                new ConsAssoc<String,Integer>("Wilma", 42,
                    new EmptyAssoc<String,Integer>())));
}|

If two association lists are structurally equal, that certainly
implies they represent the same information, but as these examples
show, two association lists that are structurally distinct can still
represent the same association.

@exercise{Design a @tt{same} method that determines if this
association list and a given association list represent the same
information. (You may need to design one or more helper methods to
solve this problem.}

You can use this notion of equality to ovveride @tt{equals} as follows:

@verbatim|{
    public boolean equals(Object that) {
	return (that instanceof Assoc)
	    && this.same((Assoc<K,V>)that);
    }
}|

Notice that must have @tt{that} be of type @tt{Object} in order to
override the built-in @tt{equals} method (contrary to what was said in
class).  You may get warnings about the cast, but they can be ignored
for now (Java is fundamentally broken when it comes to casting and
parameterized types.)

@exercise{Design a @tt{hashCode} method that is compatible with the
above notion of equality.  Your method must be able to distinguish at
least three different association list (thus always return a constant
will not suffice.)}

@lab:section{Mutable association lists}

Let's now revise the design of association lists to make them mutable,
meaning we can change values associated with a key and we can add new
key-value pairs.

Here is the interface that mutable association lists should support:

@verbatim|{
// Represents a mutable association of keys and values.
interface MAssoc<K,V> {
  // EFFECT: Add given key, value pair to this assoc.
  void add(K, V);

  // EFFECT: Update value associated with given key in this assoc.
  // Produces the old value associate with the key.
  // (Throws an exception if key is not in this assoc.)
  V update(K key, V val);

  // Get the value associated with given key in this assoc.
  // (Throws an exception if key is not in this assoc.)
  V get(K key);
}
}|

Note that it does not make sense to compare mutable hash tables for
structural equality or interpreted equality.  That's because when we
represent something with a mutable structure, we are representing
information about sharing.  So for a mutable structure, the only
sensible notion of @tt{equals} is the default one, which is
intensional equality.

@exercise{Design an implementation of the above interface.}

Here's a hint about the representation of an @tt{MAssoc}: you won't be
able to follow the same structure as for @tt{Assoc}.  Why?  The answer
has to do with @tt{add}.  Think about it and try to articulate why
this design won't work.

A design that will work is to have a mutable association contain an
@tt{Assoc} as data.  You may have to add an @tt{update} to your
@tt{Assoc} interface, however.

Regardless of your design, it should always be possible to @emph{view}
the data in a mutable association list as though it were just a plain
old association list.  Thus, it should be possible to use any
computation formulated as an @tt{AssocVisitor} as a computation over a
mutable association list, too.

@exercise{Add a @tt{<R> R accept(AssocVisitor<K,V,R> v);} method to
the @tt{MAssoc<K,V>} interface so that you can re-use any association
list computation on mutable association lists.}

