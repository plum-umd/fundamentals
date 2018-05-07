package edu.umd.cmsc132A;

import tester.Tester;

import java.util.function.Predicate;

public class Lab23 { /* Intentionally blank */ }

class Main {

  // Sum using foldr, for-each-, for-, and while-loops
  void testSum(Tester t) {
    Listof<Integer> l123 = AListof.make(3, n -> n+1);
    Integer sum = 6;

    // Ex 1a: sum using `foldr'
    Integer foldrSum = 0;  // ... modify to use `l123.foldr'
    t.checkExpect(foldrSum, sum);

    // Ex 1b: sum using for-each loop
    Integer forEachSum = 0;
    /* for (??? : ???) {
           ???
       }
     */
    t.checkExpect(forEachSum, sum);

    // Ex 1c: sum using for loop
    Integer forSum = 0;
    /* for (??? ; ??? ; ???) {
           ???
       }
     */
    t.checkExpect(forSum, sum);

    // Ex 1d: sum using while loop
    Integer whileSum = 0;
    /* while (???) {
           ???
       }
     */
    t.checkExpect(whileSum, sum);
  }

  // Exists using foldr, for-each-, for-, and while-loops
  void testExists(Tester t) {
    Listof<String> labc =
            new Cons<>("aardvark",
                    new Cons<>("brior",
                            new Cons<>("cauldron", new Empty<>())));
    Predicate<String> lt6  = s -> s.length() < 6;
    Predicate<String> hasZ = s -> s.contains("z");

    // Ex 2a: exists using `foldr'
    Boolean foldrExistsLt6  = false;  // ... modify to use `labc.foldr'
    Boolean foldrExistsHasZ = false;  // ... modify to use `labc.foldr'
    t.checkExpect(foldrExistsLt6, true);
    t.checkExpect(foldrExistsHasZ, false);

    // Ex 2b: exists using for-each loop
    Boolean forEachExistsLt6 = false;
    Boolean forEachExistsHasZ = false;
    /* for (??? : ???) {
           ???
       }
     */
    t.checkExpect(forEachExistsLt6, true);
    t.checkExpect(forEachExistsHasZ, false);

    // Ex 2c: exists using for loop
    Boolean forExistsLt6 = false;
    Boolean forExistsHasZ = false;
    /* for (??? ; ??? ; ???) {
           ???
       }
     */
    t.checkExpect(forExistsLt6, true);
    t.checkExpect(forExistsHasZ, false);

    // Ex 2d: exists using while loop
    Boolean whileExistsLt6 = false;
    Boolean whileExistsHasZ = false;
    /* while (???) {
           ???
       }
     */
    t.checkExpect(whileExistsLt6, true);
    t.checkExpect(whileExistsHasZ, false);
  }

  // Reverse using foldr, for-each-, for-, and while-loops
  void testRev(Tester t) {
    Listof<Integer> l123 = AListof.make(3, n -> n+1);
    Listof<Integer> l321 = AListof.make(3, n -> 3-n);

    // Ex 3a: rev using for-each loop
    Listof<Integer> forEachRev = AListof.empty();
    /* for (??? : ???) {
           ???
       }
     */
    t.checkExpect(forEachRev, l321);

    // Ex 3b: rev using for loop
    Listof<Integer> forRev = AListof.empty();
    /* for (??? ; ??? ; ???) {
           ???
       }
     */
    t.checkExpect(forRev, l321);

    // Ex 3c: rev using while loop
    Listof<Integer> whileRev = AListof.empty();
    /* while (???) {
           ???
       }
     */
    t.checkExpect(whileRev, l321);

    // Ex 3d: Why is this more difficult to do using `foldr'?
    // Answer:
    //
    //

  }

  // Average using foldr, for-each-, for-, and while-loops
  // See https://en.wikipedia.org/wiki/Moving_average#Cumulative_moving_average
  // for help with the looping variants.
  void testAvg(Tester t) {
    Listof<Integer> l123 = AListof.make(3, n -> n+1);
    Integer avg = 2;

    // Ex 4a: avg using `foldr'
    Integer foldrAvg  = 0;  // ... modify to use `l123.foldr'
    t.checkExpect(foldrAvg, avg);

    // Ex 4b: avg using for-each loop
    Integer forEachAvg = 0;
    /* for (??? : ???) {
           ???
       }
     */
    t.checkExpect(forEachAvg, avg);

    // Ex 4c: avg using for loop
    Integer forAvg = 0;
    /* for (??? ; ??? ; ???) {
           ???
       }
     */
    t.checkExpect(forAvg, avg);

    // Ex 4d: avg using while loop
    Integer whileAvg = 0;
    /* while (???) {
           ???
       }
     */
    t.checkExpect(whileAvg, avg);
  }

  // FizzBuzz using foldr, for-each-, for-, and while-loops
  void testFizzBuzz(Tester t) {
    Listof<Integer> l1to100 = AListof.make(100, n -> n+1);
    Listof<String> fizzBuzz = 
      AListof.make(100, n -> ((n+1) % 3) == (0) ?
                             "fizz" :
                             ((n+1) % 5) == (0) ?
                             "buzz" :
                             String.valueOf(n+1));
    System.out.println(fizzBuzz);

    // Ex 5a: fizz buzz using `foldr'
    Listof<String> foldrFizzBuzz = AListof.empty();
    t.checkExpect(foldrFizzBuzz, fizzBuzz);

    // Ex 5b: fizz buzz using for-each loop
    Listof<String> forEachFizzBuzz = AListof.empty();
    /* for (??? : ???) {
           ???
       }
     */
    t.checkExpect(forEachFizzBuzz, fizzBuzz);

    // Ex 5c: fizz buzz using for loop
    Listof<String> forFizzBuzz = AListof.empty();
    /* for (??? ; ??? ; ???) {
           ???
       }
     */
    t.checkExpect(forFizzBuzz, fizzBuzz);

    // Ex 5d: fizz buzz using while loop
    Listof<String> whileFizzBuzz = AListof.empty();
    /* while (???) {
           ???
       }
     */
  }

}
