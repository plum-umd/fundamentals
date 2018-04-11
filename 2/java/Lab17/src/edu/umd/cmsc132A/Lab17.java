package edu.umd.cmsc132A;

import tester.Tester;

import java.util.Optional;

public class Lab17 { /* Intentionally blank; leave blank */ }

//-----------------------------------------------------------------------------
// Main

class Main {

  Contact nick = new Contact("Nick", 5551234);
  Contact dvh = new Contact("DVH", 5550000);
  Contact sam = new Contact("Sam", 5553141);

  IListofContact contacts;

  void initData() {
    this.contacts = new Cons(nick, new Cons(dvh, new Cons(sam, new Mt())));
  }

  void testRemove(Tester t) {
    this.initData();

    // Test that `dvh' is a current contact.
    t.checkExpect(contacts.findPhoneNumber(dvh.name).get(), dvh.phone);
    // Remove `dvh' from the list
    contacts.removeContact(dvh.name);
    // Test that `dvh' is no longer a contact.
    t.checkExpect(contacts.findPhoneNumber(dvh.name).isPresent(), false);

    // Test removing `nick'.
    t.checkExpect(contacts.findPhoneNumber(nick.name).get(), nick.phone);
    contacts.removeContact(nick.name);
    t.checkExpect(contacts.findPhoneNumber(nick.name).isPresent(), false);
  }

}
