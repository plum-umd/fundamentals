package edu.umd.cmsc132A;

class Person {
    String name;
    Bank acct;
    Person(String name, Bank acct) {
        this.name = name;
        this.acct = acct;
    }

    // Deposit a payday check in this person's bank account
    // EFFECT: deposit's money into this person's account
    void payday() {
        // this.name = "Deena";
        this.acct.deposit(10000);
    }
}
