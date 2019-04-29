package edu.umd.cmsc132A;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Optional;

public class HashTable<K,V> implements Table<K,V> {

    ArrayList<ListTable<K,V>> entries;

    Integer arrSize = 300;
    Integer bound = 50;

    HashTable() {
        this.entries = new ArrayList<>(this.arrSize);
        for (Integer i = 0; i < this.arrSize; i++) this.entries.add(i, new ListTable<>());
    }

    public void put(K key, V val) {
        this.entries.get(key.hashCode() % this.arrSize).put(key, val);

        if (this.entries.get(hashCode() % this.arrSize).size() > this.bound) {
            // resize the table
            this.resize();
        }
    }

    // Double the size of the arraylist and relocate all key-values in this table
    public void resize() {
        Integer newSize = this.arrSize * 2;
        ArrayList<ListTable<K,V>> newEntries = new ArrayList<>(this.arrSize * 2);
        for (Integer i = 0; i < newSize; i++) newEntries.add(i, new ListTable<>());

        for (Pair<K,V> kv : this.keyvals()) {
            newEntries.get(kv.left.hashCode() % newSize).put(kv.left, kv.right);
        }
        this.entries = newEntries;
        this.arrSize = newSize;
    }


    // Lookup key in this table
    public Optional<V> lookup(K key) {
        return this.entries.get(key.hashCode() % this.arrSize).lookup(key);
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

        // return this.entries.entries;

        for (ListTable<K,V> r : this.entries) {
            // ls = ls.app(r.entries);
            for (Pair<K,V> kv : r.entries) {
                ls = new Cons<>(kv, ls);
            }
        }

        return ls;
    }

    // Clear out this table
    public void clear() {
        for (Integer i = 0; i < this.arrSize; i++) this.entries.set(i, new ListTable<>());
    }

    // Compute the number of elements in this table
    public Integer size() {
        Integer i = 0;
        for (K k : this.keys()) i++;
        return i;
    }

    public Iterator<Pair<K,V>> iterator() {
        return this.keyvals().iterator();
    }
}
