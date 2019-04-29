package edu.umd.cmsc132A;

import java.util.Optional;

public interface Table<K,V> extends Iterable<Pair<K,V>> {

    // Lookup key in this table
    Optional<V> lookup(K key);

    // Add (or update) key value pair to this table
    void put(K key, V val);

    // Does this table contain an entry for key?
    Boolean containsKey(K key);

    // Produce a list of keys, values, and pairs
    Lo<K> keys();
    Lo<V> vals();
    Lo<Pair<K,V>> keyvals();

    // Clear out this table
    void clear();

    // Compute the number of elements in this table
    Integer size();
}
