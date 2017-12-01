import tester.*;

// Represents a association between keys and values.
interface Assoc<K,V> {
    // Accept visitor and compute result.
    <R> R accept(AssocVisitor<K,V,R> v);

    // Is this assoc interpreted the same as that assoc?
    Boolean same(Assoc<K,V> that);

    // Is this a subset of that?
    Boolean subset(Assoc<K,V> that);
    // Does this assoc contain given key, value association?
    Boolean contains(K key, V val);
    // Remove this key value pair from this assoc
    Assoc<K,V> remove(K key);

    // EFFECT: modify value to given value for given key
    // Produce old value
    V update(K key, V val);
}

interface AssocVisitor<K,V,R> {
    R visitEmptyAssoc();
    R visitConsAssoc(K key, V val, Assoc<K,V> rest);
}

abstract class AAssoc<K,V> implements Assoc<K,V> {
    AAssoc() {}

    // Is this assoc interpreted the same as that assoc?
    public Boolean same(Assoc<K,V> that) {
	return this.subset(that)
	    && that.subset(this);
    }

    public boolean equals(Object that) {
	return (that instanceof Assoc)
	    && this.same((Assoc<K,V>)that);
    }
}

class EmptyAssoc<K,V> extends AAssoc<K,V> {
    EmptyAssoc() {}
    public <R> R accept(AssocVisitor<K,V,R> v) {
	return v.visitEmptyAssoc();
    }
    public Boolean subset(Assoc<K,V> that) { return true; }
    public Boolean contains(K key, V val) { return false; }
    public Assoc<K,V> remove(K key) { return this; }
    public int hashCode() { return 42; }
    public V update(K key, V val) {
	throw new RuntimeException("No such key");
    }
}

class ConsAssoc<K,V> extends AAssoc<K,V> {
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
    public Boolean subset(Assoc<K,V> that) {
	return that.contains(this.key, this.val)
	    && this.rest.remove(this.key).subset(that);
    }
    public Boolean contains(K key, V val)  {
	return (this.key.equals(key) &&
		this.val.equals(val))
	    || this.rest.contains(key, val);
    }
    public Assoc<K,V> remove(K key) {
	if (this.key.equals(key)) {
	    return this.rest.remove(key);
	} else {
	    return new ConsAssoc<K,V>(this.key, this.val,
				      this.rest.remove(key));
	}
    }
    public int hashCode() { return this.key.hashCode(); }
    public V update(K key, V val) {
	if (this.key.equals(key)) {
	    V tmp = this.val;
	    this.val = val;
	    return tmp;
	} else {
	    return this.rest.update(key, val);
	}
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

// Represents a mutable association of keys and values.
interface MAssoc<K,V> {
    <R> R accept(AssocVisitor<K,V,R> v);

    // EFFECT: Add given key, value pair to this assoc.
    void add(K key, V val);

    // EFFECT: Update value associated with given key in this assoc.
    // Produces the old value associate with the key.
    // (Throws an exception if key is not in this assoc.)
    V update(K key, V val);

    // Get the value associated with given key in this assoc.
    // (Throws an exception if key is not in this assoc.)
    V get(K key);
}

class MutAssoc<K,V> implements MAssoc<K,V> {
    Assoc<K,V> assoc;
    MutAssoc() {
	this.assoc = new EmptyAssoc<K,V>();
    }

    public <R> R accept(AssocVisitor<K,V,R> v) {
	return this.assoc.accept(v);
    }


    // EFFECT: Add given key, value pair to this assoc.
    public void add(K key, V val) {
	this.assoc = new ConsAssoc<K,V>(key, val, this.assoc);
    }

    // EFFECT: Update value associated with given key in this assoc.
    // Produces the old value associate with the key.
    // (Throws an exception if key is not in this assoc.)
    public V update(K key, V val) {
	return this.assoc.update(key, val);
    }

    // Get the value associated with given key in this assoc.
    // (Throws an exception if key is not in this assoc.)
    public V get(K key) {
	return this.assoc.accept(new Lookup<K,V>(key));
    }
}


class Examples {
    Assoc<String,Integer> mt = new EmptyAssoc<String,Integer>();

    Assoc<String,Integer> a1 =
	new ConsAssoc<String,Integer>("Fred", 40,
	    new ConsAssoc<String,Integer>("Wilma", 95,
		new EmptyAssoc<String,Integer>()));

    Assoc<String,Integer> a2 =
	new ConsAssoc<String,Integer>("Wilma", 95,
	    new ConsAssoc<String,Integer>("Fred", 40,
                new EmptyAssoc<String,Integer>()));

    Assoc<String,Integer> a3 =
	new ConsAssoc<String,Integer>("Wilma", 95,
            new ConsAssoc<String,Integer>("Fred", 40,
                new ConsAssoc<String,Integer>("Wilma", 42,
                    new EmptyAssoc<String,Integer>())));

    void testLookup(Tester t) {
	t.checkExpect(a1.accept(new Lookup<String,Integer>("Fred")), 40);
	t.checkExpect(a1.accept(new Lookup<String,Integer>("Wilma")), 95);
	t.checkExpect(a3.accept(new Lookup<String,Integer>("Fred")), 40);
	t.checkExpect(a3.accept(new Lookup<String,Integer>("Wilma")), 95);

    }

    void testSubset(Tester t) {
	t.checkExpect(mt.subset(mt), true);
	t.checkExpect(a1.subset(mt), false);
	t.checkExpect(a1.subset(a1), true);
	t.checkExpect(a1.subset(a3), true);
	t.checkExpect(a3.subset(a1), true);
    }

    void testSame(Tester t) {
	t.checkExpect(mt.same(mt), true);
	t.checkExpect(mt.same(a1), false);
	t.checkExpect(a1.same(mt), false);
	t.checkExpect(a1.same(a1), true);
	t.checkExpect(a1.same(a2), true);
	t.checkExpect(a1.same(a3), true);
	t.checkExpect(a3.same(a1), true);
    }

    void testEquals(Tester t) {
	t.checkExpect(mt.equals("Hi"), false);
	t.checkExpect(mt.equals(mt), true);
	t.checkExpect(mt.equals(a1), false);
	t.checkExpect(a1.equals(mt), false);
	t.checkExpect(a1.equals(a1), true);
	t.checkExpect(a1.equals(a2), true);
	t.checkExpect(a1.equals(a3), true);
	t.checkExpect(a3.equals(a1), true);
    }

    void testMut(Tester t) {
	MAssoc<String,Integer> a = new MutAssoc<String,Integer>();
	a.add("Fred", 45);
	t.checkExpect(a.get("Fred"), 45);
	t.checkExpect(a.update("Fred", 19), 45);
	t.checkExpect(a.get("Fred"), 19);
	t.checkExpect(a.accept(new Lookup<String,Integer>("Fred")), 19);
    }
}
