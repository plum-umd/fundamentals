import tester.*;

// Represents a association between keys and values.
interface Assoc<K,V> {
    // Accept visitor and compute result.
    <R> R accept(AssocVisitor<K,V,R> v);

    // Is this assoc structurally the same as that assoc?
    Boolean same(Assoc<K,V> that);

    // Is this assoc structurally the same as that empty assoc?
    Boolean sameEmptyAssoc(EmptyAssoc<K,V> that);

    // Is this assoc structurally the same as that cons assoc?
    Boolean sameConsAssoc(ConsAssoc<K,V> that);
}

interface AssocVisitor<K,V,R> {
    R visitEmptyAssoc();
    R visitConsAssoc(K key, V val, Assoc<K,V> rest);
}

class EmptyAssoc<K,V> implements Assoc<K,V> {
    EmptyAssoc() {}
    public <R> R accept(AssocVisitor<K,V,R> v) {
	return v.visitEmptyAssoc();
    }

    // Is this assoc structurally the same as that assoc?
    public Boolean same(Assoc<K,V> that) {
	return that.sameEmptyAssoc(this);
    }

    // Is this assoc structurally the same as that empty assoc?
    public Boolean sameEmptyAssoc(EmptyAssoc<K,V> that) {
	return true;
    }

    // Is this assoc structurally the same as that cons assoc?
    public Boolean sameConsAssoc(ConsAssoc<K,V> that) {
	return false;
    }
}

class ConsAssoc<K,V> implements Assoc<K,V> {
    K key;
    V val;
    Assoc<K,V> rest;
    ConsAssoc(K key, V val, Assoc<K,V> rest) {
	this.key = key;
	this.val = val;
	this.rest = rest;
    }
    public <R> R accept(AssocVisitor<K,V,R> v) {
	return v.visitConsAssoc(this.key, this.val, this.rest);
    }

    // Is this assoc structurally the same as that assoc?
    public Boolean same(Assoc<K,V> that) {
	return that.sameConsAssoc(this);
    }

    // Is this assoc structurally the same as that empty assoc?
    public Boolean sameEmptyAssoc(EmptyAssoc<K,V> that) {
	return false;
    }

    // Is this assoc structurally the same as that cons assoc?
    public Boolean sameConsAssoc(ConsAssoc<K,V> that) {
	return this.key.equals(that.key)
	    && this.val.equals(that.val)
	    && this.rest.same(that.rest);
    }
}

// Look up value associated with given key in visited assoc.
class Lookup<K,V> implements AssocVisitor<K,V,V> {
    K key;
    Lookup(K key) {
	this.key = key;
    }
    public V visitEmptyAssoc() {
	throw new RuntimeException("Key not found");
    }
    public V visitConsAssoc(K key, V val, Assoc<K,V> rest) {
	if (this.key.equals(key)) {
	    return val;
	} else {
	    return rest.accept(this);
	}
    }
}

class Examples {
    Assoc<String,Integer> mt = new EmptyAssoc<String,Integer>();
    Assoc<String,Integer> grades =
	new ConsAssoc<String,Integer>("Fred", 40,
	    new ConsAssoc<String,Integer>("Wilma", 95,
		new EmptyAssoc<String,Integer>()));

    void testLookup(Tester t) {
	t.checkExpect(grades.accept(new Lookup<String,Integer>("Fred")), 40);
	t.checkExpect(grades.accept(new Lookup<String,Integer>("Wilma")), 95);
    }

    void testSame(Tester t) {
	t.checkExpect(mt.same(mt), true);
	t.checkExpect(mt.same(grades), false);
	t.checkExpect(grades.same(mt), false);
	t.checkExpect(grades.same(grades), true);
    }
}
