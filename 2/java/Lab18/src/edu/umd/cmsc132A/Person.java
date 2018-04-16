package edu.umd.cmsc132A;

// represents a Person with a user name and a list of buddies
class Person {

    String username;
    Listof<Person> buddies;

    Person(String username) {
        this.username = username;
        this.buddies = AListof.empty();
    }

    // EFFECT:
    // Adds the given person to this person's buddy list.
    void addBuddy(Person that) {
        this.buddies = this.buddies.cons(that);
    }

    // Ex 3:
    // returns true if this Person has that as a direct buddy
    Boolean hasDirectBuddy(Person that) {
        return false;
    }

    // Ex 4:
    // returns the number of people that are direct buddies 
    // of both this and that person
    Integer countCommonBuddies(Person that) {
        return 0;
    }

    // Ex 5:
    // will the given person be invited to a party 
    // organized by this person?
    Boolean hasExtendedBuddy(Person that) {
        return false;
    }

    // Ex 6:
    // returns the number of people who will show up at the party
    // given by this person
    Integer partyCount(){
        return 0;
    }

}
