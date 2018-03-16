package edu.umd.cmsc132A;

import java.util.Optional;
import java.util.function.BiPredicate;
import java.util.function.Predicate;

interface Map<K, V> {
    // Add given key-value to this map
    Map<K, V> add(K k, V v);

    // Count the number of key-values in this map
    Integer count();

    // Look up the value associated with given key in this map (if it exists)
    Optional<V> lookup(K k);

    // Produce a set of key-values in this map
    Set<Pairof<K, V>> keyVals();

    // Produce a set of keys in this map
    Set<K> keys();

    // Produce a list of values in this map (order is unspecified)
    Listof<V> vals();

    // Is this map the same as m?
    // (Does it have the same keys mapping to the same values?)
    Boolean same(Map<K, V> m);

    // Remove any key-value with the given key from this map
    Map<K, V> remove(K k);

    // Remove all key-values with keys that satisfy given predicate
    Map<K, V> removeAll(Predicate<K> p);

    // Remove all key-values w/ key-values that satisfy given binary predicate
    Map<K, V> removeAllPairs(BiPredicate<K, V> p);

    // Join this map and given map (entries in this map take precedence)
    Map<K, V> join(Map<K, V> m);
}

abstract class AMap<K, V> implements Map<K, V> {
    static <J, U> Map<J, U> empty() {
        return null; // REPLACE WITH CODE TO CONSTRUCT AN EMPTY MAP
    }
}
