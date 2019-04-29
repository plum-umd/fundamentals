package edu.umd.cmsc132A;

import java.util.Iterator;
import java.util.Optional;

import tester.Tester;

public class ListTable<K,V> implements Table<K,V> {

    Lo<Pair<K,V>> entries;

    ListTable() {
        this.entries = new Empty<>();
    }

    // Lookup key in this table
    public Optional<V> lookup(K key) {
        Optional<V> mt = Optional.empty();
        return this.entries.foldr(((kvp, o)-> kvp.left.equals(key) ? Optional.of(kvp.right) : o) , mt);
    }

    // Add (or update) key value pair to this table
    public void put(K key, V val) {
        if (this.containsKey(key)) {
            this.entries = this.entries.map(kvp -> kvp.left.equals(key) ? new Pair<>(key, val) : kvp);
        } else {
            this.entries = new Cons<>(new Pair<>(key, val), this.entries);
        }
    }

    // Does this table contain an entry for key?
    public Boolean containsKey(K key) {
        return this.entries.ormap(kvp -> kvp.left.equals(key));
    }

    // Produce a list of keys, values, and pairs
    public Lo<K> keys() { return this.entries.map(kvp -> kvp.left); }
    public Lo<V> vals() { return this.entries.map(kvp -> kvp.right); }


    public Lo<Pair<K,V>> keyvals() { return this.entries; }

    // Clear out this table
    public void clear() {
        this.entries = new Empty<>();
    }

    // Compute the number of elements in this table
    public Integer size() { return this.entries.length(); }

    public Iterator<Pair<K,V>> iterator() {
        return this.entries.iterator();
    }
}

