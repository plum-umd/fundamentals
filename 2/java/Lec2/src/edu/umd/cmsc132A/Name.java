package edu.umd.cmsc132A;

class Name extends Object  implements Comparable<Name> {
    String first;
    String last;
    Name(String first, String last) {
        this.first = first;
        this.last = last;
    }

    public String toString() {
        return this.first + " " + this.last + ", aka " + super.toString();
    }


    public static String opening() {
        return "Dear ";
    }

    // Name.opening()


    public int compareTo(Name that) {
        return (this.first.length() + this.last.length()) - (that.first.length() + that.last.length());
    }


    public String greeting() {
        return Name.opening() + this.first + " " + this.last;
    }

    public String fullname() {
        return this.first + " " + this.last;
    }

    public String fullname(String prefix) {
        return prefix + " " + this.fullname();
    }

    public String fullname(Integer i) {
        return i.toString() + " " + this.fullname();
    }

    public String fullname(Object o) {
        return "object";
    }

    public boolean equals(Object n) {
        if (n instanceof Name) {
            return this.equals((Name) n);
        } else {
            return false;
        }
    }

    // public boolean equals(Object n) { ... }

    public boolean equals(Name n) {
        return this.first.equals(n.first) && this.last.equals(n.last);
    }



    // if o1.hashCode() != o2.hashCode(), then o1.equals(o2) is false.
    // A LIE: if o1.hashCode() == o2.hashCode(), then o1.equals(o2) is true.
    // if o1.equals(o2) is true, then o1.hashCode() == o2.hashCode().

    // new Name("Tom", "VH").hashCode()
    // new Name("Sam", "T").hashCode()


    public int hashCode() {
        return this.first.length() + this.last.length();
    }


}
