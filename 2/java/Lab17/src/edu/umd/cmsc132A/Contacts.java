// Authors: partner1, partner2
// Lab 17

package edu.umd.cmsc132A;

import java.util.Optional;
import java.util.function.BiFunction;

class Contact {
    String name;
    Integer phone;
    Contact(String name, Integer phone) {
        this.name = name;
        this.phone = phone;
    }
}

interface IListofContact {
    // Find the phone number of a contact with the given name,
    // if it exists.
    Optional<Integer> findPhoneNumber(String name);

    // EFFECT:
    // Remove the first contact with the given name from the
    // list.
    void removeContact(String name);
    void removeHelper(Cons prev, String name);
}

class Mt implements IListofContact {
    public Optional<Integer> findPhoneNumber(String name) {
        return Optional.empty();
    }
    public void removeContact(String name) { return; }
    public void removeHelper(Cons prev, String name) { return; }
}

class Cons implements IListofContact {

    Contact first;
    IListofContact rest;

    Cons(Contact first, IListofContact rest) {
        this.first = first;
        this.rest = rest;
    }

    public Optional<Integer> findPhoneNumber(String name) {
        if (this.first.name.equals(name)) {
            return Optional.of(this.first.phone);
        } else {
            return this.rest.findPhoneNumber(name);
        }
    }

    public void removeContact(String name) {
        this.rest.removeHelper(this, name);
    }

    public void removeHelper(Cons prev, String name) {
        return;
    }
}