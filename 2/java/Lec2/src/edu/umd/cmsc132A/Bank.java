package edu.umd.cmsc132A;

class Bank {
    Integer amt;
    Bank(Integer amt) {
        this.amt = amt;
    }

    // Deposit given amount in to this account
    // EFFECT: update this account with given deposit
    void deposit(Integer dep) {
        this.amt = this.amt + dep;
    }
}
