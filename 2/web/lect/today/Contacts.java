import tester.*;

/*
 * Smart phone contact list program, developed in class
 * for CS2510, 9:15 section (2013, Spring).
 */

// Interp: a list of contacts
interface Contacts {
    // Does a person with given name exist in this list of contacts?
    public Boolean inContacts(Name name);

    // Broadcast message to all contacts
    public Msgs broadcastPrimary(String subj, String body);
}

// Interp: the empty list of contacts
class EmptyContacts implements Contacts {
    EmptyContacts() {}

    // Does a person with given name exist in this empty list of contacts?
    public Boolean inContacts(Name name) {
	return false;
    }

    // Broadcast message to no contacts
    public Msgs broadcastPrimary(String subj, String body) {
	return new EmptyMsgs();
    }
}

// Interp: a non-empty list of contacts
class ConsContacts implements Contacts {
    Contact first;
    Contacts rest;
    ConsContacts(Contact first, Contacts rest) {
	this.first = first;
	this.rest = rest;
    }

    // Does a person with given name exist in this non-empty list of contacts?
    public Boolean inContacts(Name name) {
	/* name.first -- String
	   name.last  -- String
           this       -- ConsContacts
	   this.first -- Contact
	   this.rest  -- Contacts
	   this.rest.inContacts(Name) -- Boolean
	   this.rest.broadcastPrimary(String,String) -- Msgs
	*/
	return this.first.hasName(name)
	    || this.rest.inContacts(name);
    }

    // Broadcast message to first contact and rest of contacts
    public Msgs broadcastPrimary(String subj, String body) {
	return new ConsMsgs(this.first.sendMsgPrimary(subj,body),
			    this.rest.broadcastPrimary(subj, body));
    }
}

// Interp: a single contact
class Contact {
    Name name;
    Conn primary;
    Conns alt;
    Contact(Name name, Conn primary, Conns alt) {
	this.name = name;
	this.primary = primary;
	this.alt = alt;
    }

    // Does this contact have the given name?
    public Boolean hasName(Name name) {
	/* this.name -- Name
	   this.primary -- Conn
	   this.alt -- Conns
	   name -- Name
	*/
	return this.name.sameAs(name);
    }

    // Send message to this contact's primary address
    public Msg sendMsgPrimary(String subj, String body) {
	return this.primary.sendMsg(subj, body);
    }
}

// Interp: a full name
class Name {
    String first;
    String last;
    Name(String first, String last) {
	this.first = first;
	this.last = last;
    }

    // Is this name the same as the given name?
    public Boolean sameAs(Name name) {
	return this.first.equals(name.first)
	    && this.last.equals(name.last);
    }
}

// Interp: a connection -- a means of getting in touch by sending
// messages
interface Conn {
    // Send message to this connection
    public Msg sendMsg(String subj, String body);
}

class Email implements Conn {
    String addr;
    Email(String addr) {
	this.addr = addr;
    }

    // Send message to this email address
    public Msg sendMsg(String subj, String body) {
	return new Eml(this.addr, subj, body);
    }

}

class Phone implements Conn {
    String number;
    Phone(String number) {
	this.number = number;
    }

    // Send message to this phone number
    public Msg sendMsg(String subj, String body) {
	return new Txt(this.number, subj.concat(": ".concat(body)));
    }
}

// Interp: list of connections
interface Conns {}

class EmptyConns implements Conns {
    EmptyConns() {}
}

class ConsConns implements Conns {
    Conn first;
    ConsConns rest;
    ConsConns(Conn first, ConsConns rest) {
	this.first = first;
	this.rest = rest;
    }
}

// Interp: list of messages
interface Msgs {}

class EmptyMsgs implements Msgs {
    EmptyMsgs() {}
}

class ConsMsgs implements Msgs {
    Msg first;
    Msgs rest;
    ConsMsgs(Msg first, Msgs rest) {
	this.first = first;
	this.rest = rest;
    }
}

interface Msg {}

// Interp: text message
class Txt implements Msg {
    String number;
    String msg;
    Txt(String number, String msg) {
	this.number = number;
	this.msg = msg;
    }
}

// Interp: email message
class Eml implements Msg {
    String addr;
    String subj;
    String body;
    Eml(String addr, String subj, String body) {
	this.addr = addr;
	this.subj = subj;
	this.body = body;
    }
}

class ContactsExamples {
    Conns mt = new EmptyConns();
    Name dvh = new Name("David", "Van Horn");
    Conn dvhccs = new Email("dvanhorn@ccs.neu.edu");
    Contact dvhc = new Contact(dvh, dvhccs, new EmptyConns());
    Contacts mtContacts = new EmptyContacts();
    Contacts dvhContacts = new ConsContacts(dvhc, new EmptyContacts());

    void testInContacts(Tester t) {
	t.checkExpect(mtContacts.inContacts(dvh), false);
	t.checkExpect(dvhContacts.inContacts(dvh), true);
	t.checkExpect(dvhContacts.inContacts(new Name("Andre", "the Giant")), false);
    }

    void testHasName(Tester t) {
	t.checkExpect(dvhc.hasName(dvh), true);
	t.checkExpect(dvhc.hasName(new Name("Andre", "the Giant")), false);
    }

    void testSendMsg(Tester t) {
	t.checkExpect(dvhccs.sendMsg("Snow", "No class"),
		      new Eml("dvanhorn@ccs.neu.edu", "Snow", "No class"));
	t.checkExpect(new Phone("6178653124").sendMsg("Snow", "No class"),
		      new Txt("6178653124", "Snow: No class"));
    }

    void testSendMsgPrimary(Tester t) {
	t.checkExpect(dvhc.sendMsgPrimary("Sun", "No class"),
		      new Eml("dvanhorn@ccs.neu.edu", "Sun", "No class"));
    }

    void testBroadcastPrimary(Tester t) {
	t.checkExpect(dvhContacts.broadcastPrimary("Son", "Come to class"),
		      new ConsMsgs(new Eml("dvanhorn@ccs.neu.edu", "Son", "Come to class"),
				   new EmptyMsgs()));
    }
}
