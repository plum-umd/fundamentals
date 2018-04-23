package edu.umd.cmsc132A;

import java.util.function.Predicate;

interface Json {
    Boolean isEquals(Json j);

    Boolean isEqualsInt(JInt ji);

    Boolean isEqualsLoJson(LoJson loj);
}

class JInt implements Json {
    Integer i;

    JInt(Integer i) {
        this.i = i;
    }

    public Boolean isEquals(Json j) {
        return j.isEqualsInt(this);
    }

    public Boolean isEqualsInt(JInt ji) {
        return this.i.equals(ji.i);
    }

    public Boolean isEqualsLoJson(LoJson loj) {
        return false;
    }
}

interface LoJson extends Json {
    Integer length();

    Boolean exists(Predicate<Json> p);

    Boolean forAll(Predicate<Json> p);
}

class Mt implements LoJson {
    public Boolean isEquals(Json j) {
        return j.isEqualsLoJson(this);
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

    public Boolean isEqualsInt(JInt ji) {
        return false;
    }

    public Boolean isEqualsLoJson(LoJson loj) {
        return this.length().equals(loj.length())
                && loj.forAll(e2 -> this.exists(e1 -> e1.isEquals(e2)));
    }
}


class JCons implements LoJson {
    Json first;
    LoJson rest;

    JCons(Json first, LoJson rest) {
        this.first = first;
        this.rest = rest;
    }

    public Boolean isEquals(Json j) {
        return j.isEqualsLoJson(this);
    }

    public Integer length() {
        return 1 + this.rest.length();
    }

    public Boolean exists(Predicate<Json> p) {
        return p.test(this.first)
                || this.rest.exists(p);
    }

    public Boolean forAll(Predicate<Json> p) {
        return p.test(this.first)
                && this.rest.forAll(p);
    }

    public Boolean isEqualsInt(JInt ji) {
        return false;
    }

    public Boolean isEqualsLoJson(LoJson loj) {
        return this.length().equals(loj.length())
                && loj.forAll(e2 -> this.exists(e1 -> e1.isEquals(e2)));
    }
}

















