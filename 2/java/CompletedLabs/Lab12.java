package edu.umd.cmsc132A;

import tester.*;

import java.util.function.BiPredicate;
import java.util.function.Predicate;

public class Lab12 {}

// This interface doesn't need to change.
interface IJson {
    Boolean equals(Json that);
}

// Ex 1:
abstract class Json implements IJson {

    public Boolean isEqual(JInt ji) {
        return false;
    }

    public Boolean isEqual(JStr js) {
        return false;
    }

    public Boolean isEqual(MtAssoc mta) {
        return false;
    }

    public Boolean isEqual(ConsAssoc ca) {
        return false;
    }

    public Boolean isEqual(Mt c) {
        return false;
    }

    public Boolean isEqual(Cons c) {
        return false;
    }

}

/* A Json is one of:
 * - new JInt(Integer)
 * - new JStr(String)
 * - Assoc
 * - LoJson
 * Interp: JSON data
 */

class JInt extends Json {
    Integer i;
    JInt(Integer i) {
        this.i = i;
    }

    public Boolean equals(Json j) {
        return j.isEqual(this);
    }

    // Ex 2:
    public Boolean isEqual(JInt ji) {
        return this.i.equals(ji.i);
    }
}

class JStr extends Json {
    String s;
    JStr(String s) {
        this.s = s;
    }

    public Boolean equals(Json j) {
        return j.isEqual(this);
    }

    // Ex 2:
    public Boolean isEqual(JStr js) {
        return this.s.equals(js.s);
    }
}

/* A LoJson is one of:
 * - new Mt()
 * - new Cons(Json first, LoJson rest)
 * Interp: A list of Json data
 */

abstract class LoJson extends Json {
    public abstract Integer length();
    public abstract Boolean forAll(Predicate<Json> p);
    public abstract Boolean exists(Predicate<Json> p);
}

class Mt extends LoJson {
    Mt(){}

    public Boolean equals(Json j) {
        return j.isEqual(this);
    }

    // Ex 2:
    public Boolean isEqual(Mt mt) {
        return true;
    }

    public Integer length() {
        return 0;
    }

    public Boolean exists(Predicate<Json> p) {
        return false;
    }

    public Boolean forAll(Predicate<Json> p) {
        return true;
    }
}

class Cons extends LoJson {
    Json first;
    LoJson rest;

    Cons(Json first, LoJson rest) {
        this.first = first;
        this.rest = rest;
    }

    public Boolean equals(Json j) {
        return j.isEqual(this);
    }

    // Ex 2:
    /* Order-dependent version
    public Boolean isEqual(Cons c) {
        // return false;
        return this.first.equals(c.first)
                && this.rest.equals(c.rest);
    }*/

    // Order-independent version
    public Boolean isEqual(Cons c) {
        return this.length() == c.length()
                && this.forAll(j0 -> c.exists(j1 -> j0.equals(j1)));
    }

    public Integer length() {
        // return 0;
        return 1 + this.rest.length();
    }

    public Boolean exists(Predicate<Json> p) {
        // return false;
        return p.test(this.first) || this.rest.exists(p);
    }

    public Boolean forAll(Predicate<Json> p) {
        // return false;
        return p.test(this.first) && this.rest.forAll(p);
    }
}

/* An Assoc is one of:
 * - new MtAssoc()
 * - new ConsAssoc(String key, Json value, Assoc rest)
 * Interp: associations between string keys and Json values
 */

abstract class Assoc extends Json {
    public abstract Integer length();

    public abstract Boolean forAll(BiPredicate<String, Json> p);

    public abstract Boolean exists(BiPredicate<String, Json> p);
}

class MtAssoc extends Assoc {
    MtAssoc(){}

    public Boolean equals(Json j) {
        // return false;
        return j.isEqual(this);
    }

    // Ex 2:
    public Boolean isEqual(MtAssoc mta) {
        return true;
    }

    public Integer length() {
        return 0;
    }

    public Boolean exists(BiPredicate<String, Json> p) {
        return false;
    }

    public Boolean forAll(BiPredicate<String, Json> p) {
        return true;
    }
}

class ConsAssoc extends Assoc {
    String key;
    Json value;
    Assoc rest;

    ConsAssoc(String key, Json value, Assoc rest) {
        this.key = key;
        this.value = value;
        this.rest = rest;
    }

    public Boolean equals(Json j) {
        return false;
        // return j.isEqual(this);
    }

    /* Order-dependent version
    public Boolean isEqual(ConsAssoc c) {
        return this.key.equals(c.key)
                && this.value.equals(c.value)
                && this.rest.equals(c.rest);
    }*/

    // Ex 2:
    // Order-independent version
    public Boolean isEqual(ConsAssoc c) {
        return this.length() == c.length()
                && this.forAll(
                        (k0, v0) -> c.exists(
                        (k1, v1) -> k0.equals(k1) && v0.equals(v1)));
    }

    public Integer length() {
        return 1 + this.rest.length();
    }

    public Boolean exists(BiPredicate<String, Json> p) {
        return p.test(this.key, this.value) || this.rest.exists(p);
    }

    public Boolean forAll(BiPredicate<String, Json> p) {
        return p.test(this.key, this.value) && this.rest.forAll(p);
    }
}

class Main {

    Json i0 = new JInt(0);
    Json i42 = new JInt(42);

    Json sfoo = new JStr("foo");
    Json sbar = new JStr("bar");

    Boolean testInt(Tester t) {
        return t.checkExpect(i0.equals(i42), false)
                && t.checkExpect(i42.equals(sfoo), false)
                && t.checkExpect(i0.equals(lc2), false)
                && t.checkExpect(i42.equals(i42), true)
                && t.checkExpect(i42.equals(new JInt(42)), true)
                && t.checkExpect(new JInt(42).equals(i42), true);
    }

    Boolean testStr(Tester t) {
        return t.checkExpect(sfoo.equals(sbar), false)
                && t.checkExpect(sbar.equals(sfoo), false)
                && t.checkExpect(sbar.equals(lc2), false)
                && t.checkExpect(sfoo.equals(sfoo), true)
                && t.checkExpect(sfoo.equals(new JStr("foo")), true)
                && t.checkExpect(new JStr("foo").equals(sfoo), true);
    }

    LoJson mt = new Mt();
    LoJson lc1 = new Cons(sfoo, mt);
    LoJson lc2 = new Cons(i0, lc1);
    LoJson cl1 = new Cons(i0, mt);
    LoJson cl2 = new Cons(sfoo, cl1);

    Boolean testList(Tester t) {
        return t.checkExpect(mt.equals(sfoo), false)
                && t.checkExpect(lc1.equals(i0), false)
                && t.checkExpect(cl2.equals(ac2), false)
                && t.checkExpect(mt.equals(mt), true)
                && t.checkExpect(mt.equals(new Mt()), true)
                && t.checkExpect(new Mt().equals(mt), true)
                && t.checkExpect(lc2.equals(lc2), true)
                && t.checkExpect(lc2.equals(new Cons(i0, new Cons(sfoo, mt))),
                true)
                && t.checkExpect(new Cons(i0, new Cons(sfoo, mt)).equals(lc2),
                true)
                && t.checkExpect(lc1.equals(new Cons(sfoo, new Cons(sfoo, mt))),
                false);
    }

    String k0 = "shuffle";
    String k1 = "duffle";
    Assoc mta = new MtAssoc();
    Assoc ca1 = new ConsAssoc(k0, i0, mta);
    Assoc ca2 = new ConsAssoc(k1, sbar, ca1);
    Assoc ac1 = new ConsAssoc(k1, sbar, mta);
    Assoc ac2 = new ConsAssoc(k0, i0, ac1);

    Boolean testAssoc(Tester t) {
        return t.checkExpect(mta.equals(sfoo), false)
                && t.checkExpect(ac1.equals(i0), false)
                && t.checkExpect(ca2.equals(lc2), false)
                && t.checkExpect(mta.equals(mta), true)
                && t.checkExpect(mta.equals(new MtAssoc()), true)
                && t.checkExpect(new MtAssoc().equals(mta), true)
                && t.checkExpect(ca2.equals(ca2), true)
                && t.checkExpect(ca2.equals(new ConsAssoc(k1, sbar,
                        new ConsAssoc(k0, i0, mta))),
                true)
                && t.checkExpect(new ConsAssoc(k1, sbar,
                        new ConsAssoc(k0, i0, mta)).equals(ca2),
                true)
                && t.checkExpect(ca1.equals(new ConsAssoc(k0, i0,
                        new ConsAssoc(k0, i0, mta))),
                false);
    }

}
