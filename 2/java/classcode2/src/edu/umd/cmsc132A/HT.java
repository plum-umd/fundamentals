package edu.umd.cmsc132A;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Optional;
import java.util.function.Consumer;

// Hashtables with keys K and values V
interface HT<K, V> {
    // Lookup the value associated with k
    Optional<V> get(K k);

    // EFFECT: update the table to map k to v
    // Produces the previous value mapped to k if any
    Optional<V> put(K k, V v);
}

class HTBucketList<K, V> implements HT<K, V> {
    ArrayList<HTList<K, V>> buckets;

    Integer size = 500;

    final Integer maxBucketSize = 2;

    HTBucketList() {
        this.init();
    }

    void init() {
        this.buckets = new ArrayList<>();
        for (Integer i = this.size; !i.equals(0); i = i - 1)
            this.buckets.add(new HTList<>());
    }

    public Optional<V> get(K k) {
        return this.buckets.get(k.hashCode() % this.size).get(k);
    }

    public Optional<V> put(K k, V v) {
        HTList<K, V> bucket = this.buckets.get(k.hashCode() % this.size);
        Optional<V> r = bucket.put(k, v);
        if (bucket.length() > this.maxBucketSize)
            this.resize();
        return r;
    }

    void resize() {
        ArrayList<HTList<K, V>> oldbuckets = this.buckets;
        this.size = this.size * 2;
        this.init();

        for (HTList<K, V> b : oldbuckets)
            for (Pairof<K, V> kv : b)
                this.put(kv.left, kv.right);

    }
}

// This class is broken and is used just for demonstrative purposes
// on the way from HTList to HTBucketList
class HTArrayList<K, V> implements HT<K, V> {
    ArrayList<Optional<V>> vals;

    Integer size = 500;

    HTArrayList() {
        this.vals = new ArrayList<>();
        this.init(this.size);
    }

    void init(Integer i) {
        if (i.equals(0)) {
            return;
        } else {
            this.vals.add(Optional.empty());
            this.init(i - 1);
        }
    }

    public Optional<V> get(K k) {
        return this.vals.get(k.hashCode() % this.size);
    }

    public Optional<V> put(K k, V v) {
        Optional<V> old = this.vals.get(k.hashCode() % this.size);
        this.vals.set(k.hashCode() % this.size, Optional.of(v));
        return old;
    }
}


class HTList<K, V> implements HT<K, V>, Iterable<Pairof<K, V>> {
    Listof<Pairof<K, V>> table;

    HTList() {
        this.table = new Empty<>();
    }

    public Iterator<Pairof<K, V>> iterator() {
        return this.table.iterator();
    }

    public <Y> Y accept(ListVisitor<Y,Pairof<K,V>> v) {
        return this.table.accept(v);
    }

    public Integer length() {
        return this.table.length();
    }

    public Optional<V> get(K k) {
        return this.table.foldr((p, ov) -> k.equals(p.left)
                        ? Optional.of(p.right)
                        : ov,
                Optional.empty());
    }

    public Optional<V> put(K k, V v) {
        Optional<V> res = this.table.foldr((p, ov) ->
                {
                    if (k.equals(p.left)) {
                        V old = p.right;
                        p.right = v;
                        return Optional.of(old);
                    } else
                        return ov;
                },
                Optional.empty());
        if (res.isPresent()) {
            return res;
        } else {
            this.table = new Cons<>(new Pairof<>(k, v), this.table);
            return Optional.empty();
        }
    }
}
