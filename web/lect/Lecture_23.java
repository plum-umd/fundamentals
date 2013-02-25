import java.util.ArrayList;
import tester.*;

/** Our own home-brew HashTable */
class Hasher<V>{
    // A List of Lists with direct access
    ArrayList<ArrayList<Pair<V>>> alist =
        new ArrayList<ArrayList<Pair<V>>>();
    int capacity;

    /** Create a Hasher with the given capacity */
    Hasher(int capacity){
        this.capacity = capacity;
        // Mark all places as Empty
        for(int i = 0; i < capacity; i++)
            alist.add(new ArrayList<Pair<V>>());
    }

    /** Return the index of the given integer in our List */
    int hash(int i){
        return i % this.capacity;
    }

    /** Does this Hasher contain the given Key? */
    boolean contains(int key){
        for(Pair p : alist.get(hash(key))){
            if(p.key == key)
                return true;
        }
        return false;
    }

    /** Put a mapping for [key -> val] into this Hasher */
    void put(int key, V val){
        // Duplicates are ok for simplicity
        alist.get(hash(key)).add(new Pair<V>(key, val));
    }

    /** Get the Value associated with the given Key */
    V get(int key){
        for(Pair<V> p : alist.get(hash(key))){
            if(p.key == key)
                return p.val;
        }
        throw new RuntimeException("No Mapping for: "+key);
    }
}

/** Represents a pair of a Key/Value */
class Pair<V>{
    // Just int for simplicity
    int key;
    V val;

    Pair(int i, V v){
        this.key = i;
        this.val = v;
    }

    /** Compute this Pair's hasCode */
    public int hashCode(){
        return this.key % 123457;
    }
    /** Is this Pair equal to the given Object? */
    public boolean equals(Object o){
        // Cast to Pair... possible ClassCastException at
        //   runtime.
        Pair<V> apear = (Pair<V>)o;

        return apear.key == this.key &&
               apear.val.equals(this.val);
    }
}

/** Represents a Person. Just an example of a computed HashCode */
class Person{
    String name;
    String eyecolor;
    boolean babyCarriageHuh;

    Person(String name, String eyecolor, boolean babyCarriageHuh) {
        this.name = name;
        this.eyecolor = eyecolor;
        this.babyCarriageHuh = babyCarriageHuh;
    }
    /** Compute a hashCode for this Person */
    public int hashCode(){
        if(this.babyCarriageHuh){
            return this.name.hashCode();
        }else{
            return this.eyecolor.hashCode() + this.name.hashCode();
        }
    }
    /** Is this Person equal to the given Object? */
    public boolean equals(Object o){
        Person p = (Person)o;

        return (p.babyCarriageHuh == this.babyCarriageHuh) &&
               (p.name.equals(this.name)) &&
               (p.eyecolor.equals(this.eyecolor));
    }
}

/** Examples for HashCode etc... */
class LectureExamples{

    void testHasher(Tester t){
        Hasher<String> hasher = new Hasher<String>(37);

        hasher.put(45, "forty-five");
        hasher.put(53, "fifty-three");
        hasher.put(12, "twelve");
        hasher.put(24, "twenty-four");
        hasher.put(61, "sixty-one");
        hasher.put(27, "twenty-seven");
        hasher.put(19, "nine-teen");

        t.checkExpect(hasher.get(12), "twelve");
        t.checkExpect(hasher.get(27), "twenty-seven");
        t.checkExpect(hasher.get(53), "fifty-three");
        t.checkExpect(hasher.get(61), "sixty-one");

    }

}






