package edu.umd.cmsc132A;

import tester.*;

public class Lab12 {}

// This interface doesn't need to change.
interface IJson {
    // Is this document structurally the same as given document?
    Boolean isSame(IJson that);

    // Is this document structurally the same as given JInt?
    Boolean isSameJInt(JInt that);
}

// Common code for all IJson classes
abstract class Json implements IJson {

    public Boolean isSameJInt(JInt ji) {
        return false;
    }

}

/* A IJson is one of:
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

    public Boolean isSame(IJson j) {
        return j.isSame(this);
    }

    public Boolean isSameJInt(JInt ji) {
        return false;
    }
}

class JStr extends Json {
    String s;
    JStr(String s) {
        this.s = s;
    }

    public Boolean isSame(IJson j) {
        return j.isSame(this);
    }
}

/* A LoJson is one of:
 * - new Mt()
 * - new Cons(Json first, LoJson rest)
 * Interp: A list of Json data
 */

abstract class LoJson extends Json {
    public abstract Integer length();
}

class Mt extends LoJson {
    Mt(){}

    public Integer length() {
        return 0;
    }
}

class Cons extends LoJson {
    Json first;
    LoJson rest;

    Cons(Json first, LoJson rest) {
        this.first = first;
        this.rest = rest;
    }

    public Integer length() {
        return 1 + this.rest.length();
    }
}

/* An Assoc is one of:
 * - new MtAssoc()
 * - new ConsAssoc(String key, Json value, Assoc rest)
 * Interp: associations between string keys and Json values
 */

abstract class Assoc extends Json {
    public abstract Integer length();
}

class MtAssoc extends Assoc {
    MtAssoc(){}

    public Integer length() {
        return 0;
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

    public Integer length() {
        return 1 + this.rest.length();
    }
}

class Main {

    Json i0 = new JInt(0);
    Json i42 = new JInt(42);

    Json sfoo = new JStr("foo");
    Json sbar = new JStr("bar");

    Boolean testInt(Tester t) {
        return t.checkExpect(i0.isSame(i42), false)
                && t.checkExpect(i42.isSame(sfoo), false)
                && t.checkExpect(i0.isSame(lc2), false)
                && t.checkExpect(i42.isSame(i42), true)
                && t.checkExpect(i42.isSame(new JInt(42)), true)
                && t.checkExpect(new JInt(42).isSame(i42), true);
    }

    Boolean testStr(Tester t) {
        return t.checkExpect(sfoo.isSame(sbar), false)
                && t.checkExpect(sbar.isSame(sfoo), false)
                && t.checkExpect(sbar.isSame(lc2), false)
                && t.checkExpect(sfoo.isSame(sfoo), true)
                && t.checkExpect(sfoo.isSame(new JStr("foo")), true)
                && t.checkExpect(new JStr("foo").isSame(sfoo), true);
    }

    LoJson mt = new Mt();
    LoJson lc1 = new Cons(sfoo, mt);
    LoJson lc2 = new Cons(i0, lc1);
    LoJson cl1 = new Cons(i0, mt);
    LoJson cl2 = new Cons(sfoo, cl1);

    Boolean testList(Tester t) {
        return t.checkExpect(mt.isSame(sfoo), false)
                && t.checkExpect(lc1.isSame(i0), false)
                && t.checkExpect(cl2.isSame(ac2), false)
                && t.checkExpect(mt.isSame(mt), true)
                && t.checkExpect(mt.isSame(new Mt()), true)
                && t.checkExpect(new Mt().isSame(mt), true)
                && t.checkExpect(lc2.isSame(lc2), true)
                && t.checkExpect(lc2.isSame(new Cons(i0, new Cons(sfoo, mt))),
                true)
                && t.checkExpect(new Cons(i0, new Cons(sfoo, mt)).isSame(lc2),
                true)
                && t.checkExpect(lc1.isSame(new Cons(sfoo, new Cons(sfoo, mt))),
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
        return t.checkExpect(mta.isSame(sfoo), false)
                && t.checkExpect(ac1.isSame(i0), false)
                && t.checkExpect(ca2.isSame(lc2), false)
                && t.checkExpect(mta.isSame(mta), true)
                && t.checkExpect(mta.isSame(new MtAssoc()), true)
                && t.checkExpect(new MtAssoc().isSame(mta), true)
                && t.checkExpect(ca2.isSame(ca2), true)
                && t.checkExpect(ca2.isSame(new ConsAssoc(k1, sbar,
                        new ConsAssoc(k0, i0, mta))),
                true)
                && t.checkExpect(new ConsAssoc(k1, sbar,
                        new ConsAssoc(k0, i0, mta)).isSame(ca2),
                true)
                && t.checkExpect(ca1.isSame(new ConsAssoc(k0, i0,
                        new ConsAssoc(k0, i0, mta))),
                false);
    }

}
