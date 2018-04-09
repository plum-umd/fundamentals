package edu.umd.cmsc132A;

import java.util.Optional;
import java.util.function.BiPredicate;
import java.util.function.Predicate;

// Similar to a Map, but requires keys implement Comparable interface
interface MapComp<K extends Comparable<K>, V> {

    // Add given key-value to this map
    MapComp<K, V> add(K k, V v);

    // Count the number of key-values in this map
    Integer count();

    // Look up the value associated with given key in this map (if it exists)
    Optional<V> lookup(K k);

    // Produce a set of key-values in this map
    Set<Pairof<K, V>> keyVals();

    // Produce a list of key-values in ascending sorted order
    Listof<Pairof<K,V>> sortedKeyVals();

    // Produce a set of keys in this map
    Set<K> keys();

    // Produce a set of keys in ascending sorted order
    Listof<K> sortedKeys();

    // Produce a list of values in this map (order is unspecified)
    Listof<V> vals();

    // Produce a list of values in the ascending sorted order
    // of their keys in this map
    // NOTE: list is not itself sorted
    Listof<V> sortedVals();

    // Is this map the same as m?
    // (Does it have the same keys mapping to the same values?)
    Boolean same(MapComp<K, V> m);

    // Remove any key-value with the given key from this map
    MapComp<K, V> remove(K k);

    // Remove all key-values with keys that satisfy given predicate
    MapComp<K, V> removeAll(Predicate<K> p);

    // Remove all key-values w/ key-values that satisfy given binary predicate
    MapComp<K, V> removeAllPairs(BiPredicate<K, V> p);

    // Join this map and given map (entries in this map take precedence)
    MapComp<K, V> join(MapComp<K, V> m);
}

// Implementation of MapComp that uses a BST for efficient operations
class MapBST<K extends Comparable<K>, V> implements MapComp<K, V> {

    // A Pairof where comparisons are done on the left component
    class KVPairof<K extends Comparable<K>, V> extends Pairof<K, V>
            implements Comparable<KVPairof<K, V>> {

        KVPairof(K left, V right) {
            super(left, right);
        }

        public int compareTo(KVPairof<K, V> p) {
            return this.left.compareTo(p.left);
        }
    }

    BST<KVPairof<K, V>> bst;

    MapBST() {
        this.bst = new Leaf<>();
    }

    // Add given key-value to this map
    public MapComp<K, V> add(K k, V v) {
        return null;
    }

    // Count the number of key-values in this map
    public Integer count() {
        return null;
    }

    // Look up the value associated with given key in this map (if it exists)
    public Optional<V> lookup(K k) {
        return null;
    }

    // Produce a set of key-values in this map
    public Set<Pairof<K, V>> keyVals() {
        return null;
    }

    // Produce a list of key-values in ascending sorted order
    public Listof<Pairof<K,V>> sortedKeyVals() {
        return null;
    }

    // Produce a set of keys in this map
    public Set<K> keys() {
        return null;
    }

    // Produce a set of keys in ascending sorted order
    public Listof<K> sortedKeys() {
        return null;
    }

    // Produce a list of values in this map (order is unspecified)
    public Listof<V> vals() {
        return null;
    }

    // Produce a list of values in the ascending sorted order
    // of their keys in this map
    // NOTE: list is not itself sorted
    public Listof<V> sortedVals() {
        return null;
    }

    // Is this map the same as m?
    // (Does it have the same keys mapping to the same values?)
    public Boolean same(MapComp<K, V> m) {
        return null;
    }

    // Remove any key-value with the given key from this map
    public MapComp<K, V> remove(K k) {
        return null;
    }

    // Remove all key-values with keys that satisfy given predicate
    public MapComp<K, V> removeAll(Predicate<K> p) {
        return null;
    }

    // Remove all key-values w/ key-values that satisfy given binary predicate
    public MapComp<K, V> removeAllPairs(BiPredicate<K, V> p) {
        return null;
    }

    // Join this map and given map (entries in this map take precedence)
    public MapComp<K, V> join(MapComp<K, V> m) {
        return null;
    }
}
