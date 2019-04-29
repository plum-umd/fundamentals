package edu.umd.cmsc132A;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Optional;

public class HashTableBroken<K,V> implements Table<K,V> {

    ArrayList<Optional<Pair<K,V>>> entries;

    HashTableBroken() {
        this.entries = new ArrayList<>(100);
        for (Integer i = 0; i < 100; i++) this.entries.add(i, Optional.empty());
    }

    public void put(K key, V val) {
        this.entries.set(key.hashCode() % 100, Optional.of(new Pair<>(key, val)));
    }

    // Lookup key in this table
    public Optional<V> lookup(K key) {
        Optional<Pair<K,V>> r = this.entries.get(key.hashCode() % 100);
        if (r.isPresent()) {
            return Optional.of(r.get().right);
        } else {
            return Optional.empty();
        }
    }

    // Does this table contain an entry for key?
    public Boolean containsKey(K key) {
        return this.lookup(key).isPresent();
    }

    // Produce a list of keys, values, and pairs
    public Lo<K> keys() {
        return this.keyvals().map(kv -> kv.left);
    }


    public Lo<V> vals() {
        return this.keyvals().map(kv -> kv.right);
    }

    public Lo<Pair<K,V>> keyvals() {
        Lo<Pair<K,V>> ls = new Empty<>();
        for (Optional<Pair<K,V>> r : this.entries) {
            if (r.isPresent()) {
                ls = new Cons<>(r.get(), ls);
            }
        }
        return ls;

    }

    // Clear out this table
    public void clear() {
        for (Integer i = 0; i < 100; i++) this.entries.set(i, Optional.empty());
    }

    // Compute the number of elements in this table
    public Integer size() {
        Integer i = 0;
        for (K k : this.keys()) i++;
        return i;
    }

    public Iterator<Pair<K,V>> iterator() { return null; }
}