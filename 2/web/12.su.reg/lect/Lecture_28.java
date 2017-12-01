/***********************************************
 *  Sorting Demonstration(s)
 ***********************************************/

import java.util.*;
import tester.*;
import image.*;
import world.*;

/** Demonstration of Insertion Sort */
class Insertion extends ShowSort {
    /** The partition of sorted/unsorted elements */
    Integer part;

    /** Take one step of this sorting algorithm */
    void sortStep() {
        insert(this.part);
        this.part++;
        this.current = this.part;
        if (part.equals(this.alist.size()))
            this.done = true;
    }

    /** Insert the element at the given index into place
     *    at the front of the list */
    void insert(Integer idx){
        Integer i = 0;
        while (i < idx && alist.get(idx) < alist.get(i))
            i++;
        alist.add(i, alist.remove(idx));
    }
    /** The Name of this Sorting Algorithm */
    String name() { return "Insertion Sort"; }
    /** Start with the partition at 0 */
    void init(Integer size){
        super.init(size);
        this.part = 0;
    }
}

/** Demonstration of Selection Sort */
class Selection extends Insertion {
    /** Swap the next maximum with the first of the
     *    unsorted partition */
    void sortStep() {
        swap(findMaxIdx(this.alist), this.part);
        this.part++;
        this.current = findMaxIdx(this.alist);
        if (part == this.alist.size())
            this.done = true;
    }
    /** Compute the index of the maximum element */
    int findMaxIdx(ArrayList<Integer> alist){
        int maxI = this.part;
        for(int i = maxI; i < alist.size(); i++)
            if(alist.get(i) > alist.get(maxI))
                maxI = i;
        return maxI;
    }
    /** The Name of this Sorting Algorithm */
    String name(){ return "Selection Sort"; }
    /** First current is the maximum */
    void init(int size){
        super.init(size);
        this.current = findMaxIdx(this.alist);
    }
}

/** Demonstration of Sort */
class Bubble extends Insertion{
    /** Take one step of this sorting algorithm */
    void sortStep(){
        this.bubbleOne(this.alist);
        this.current = this.part;
        this.part++;
        if(part <= 0)
            this.done = true;
    }
    /** Bubble one element into place */
    void bubbleOne(ArrayList<Integer> alist){
        for(int i = this.alist.size()-1; i > this.part; i--){
            if(alist.get(i) > alist.get(i-1))
                swap(i, i-1);
        }
    }
    /** The Name of this Sorting Algorithm */
    String name(){ return "Bubble Sort"; }
}

/** Demonstration of Merge Sort */
class Merge extends ShowSort{
    /** The size of merge groups */
    int group;

    /** Take one step of this sorting algorithm */
    void sortStep(){
        this.mergeSteps(this.alist);
        this.group *= 2;
        if(group >= this.alist.size())
            this.done = true;
    }
    /** Merge group sized sub-lists */
    void mergeSteps(ArrayList<Integer> alist){
        int b = 0;
        while(b < alist.size()){
            int i = b;
            int j = b+this.group;
            while(j < b+2*this.group && i < alist.size() && i < j){
                if(j < this.alist.size() && alist.get(i) < alist.get(j)){
                    alist.add(i, alist.remove(j));
                    j++;
                }
                i++;
            }
            b += 2*this.group;
        }
    }
    /** Draw is different, so we show the current merge groups */
    public Scene onDraw(){
        Scene scn = super.onDraw();
        if(!this.vis || this.done)return scn;
        int b = 0;
        while(b < alist.size()){
            int y1 = b*15+14;
            int y2 = Math.min(this.alist.size(), (b+this.group))*15+12;
            scn = scn.placeImage(new Rectangle(550, y2-y1, "outline", "red"),
                    278, (y1+y2)/2-1);
            b += group;
        }
        return scn;
    }
    /** The Name of this Sorting Algorithm */
    String name(){ return "Merge Sort"; }
    /** Group size starts from 1 */
    void init(int size){
        this.group = 1;
        this.current = -1;
        super.init(size);
    }
}

/** Represents a partition to be recursively sorted */
class P {
    int s; int e;
    P(int s, int e){ this.s = s; this.e = e; }
}
/** Demonstration of Sort */
class Quick extends ShowSort{
    /** The groups to be recursively sorted */
    ArrayList<P> groups;

    /** Take one step of this sorting algorithm */
    void sortStep(){
        this.quickSteps(this.alist, this.groups);
        if(this.groups.isEmpty())
            this.done = true;
    }
    /** "Recursively" sort each partition in groups */
    void quickSteps(ArrayList<Integer> alist, ArrayList<P> gs){
        this.groups = new ArrayList<P>();
        // For each group, partition it, then setup for next time
        for(P p : gs){
            // Something to partition
            if(p.s+2 < p.e){
                int bar = p.s+1;
                int piv = alist.get(p.s);
                for(int i = p.s+1; i < p.e; i++){
                    if(alist.get(i) > piv){
                        swap(bar, i);
                        bar++;
                    }
                }
                swap(p.s, bar-1);
                this.groups.add(new P(p.s, bar));
                this.groups.add(new P(bar, p.e));
            }
        }
    }
    /** Draw boxes for all the recursive partitions */
    public Scene onDraw(){
        Scene scn = super.onDraw();
        if(!this.vis || this.done)return scn;
        for(P p : this.groups){
            int y1 = p.s*15+14;
            int y2 = (p.e-1)*15+12;
            scn = scn.placeImage(new Rectangle(550, y2-y1, "outline", "red"),
                    278, (y1+y2)/2-1);
        }
        return scn;
    }
    /** The Name of this Sorting Algorithm */
    String name(){ return "QuickSort"; }
    /** Current never shows... start with the whole list */
    void init(int size){
        super.init(size);
        current = -1;
        this.groups = new ArrayList<P>();
        this.groups.add(new P(0, this.alist.size()));
    }
}


/** Demonstration of Sort */
abstract class ShowSort extends VoidWorld{
    ArrayList<Integer> alist = new ArrayList<Integer>();
    boolean done;
    boolean paused;
    boolean show;
    int current;

    /** Create a random list of Numbers */
    ShowSort(){ init(); }

    /** Speed of one step */
    public double tickRate(){ return 0.2; }

    /** The Name of this Sorting Algorithm */
    abstract String name();

    /** Draw all the numbers as Bars... */
    public Scene onDraw(){
        Scene scn = new EmptyScene(560, 550);
        for(int i = 0; i < this.alist.size(); i++){
            scn = scn.placeImage(new Rectangle(this.alist.get(i)*10+10, 10, "solid",
                    (i == this.current && !this.done && this.vis) ? "red" : "blue"),
                                 this.alist.get(i)*5+10, i*15+20);
        }
        if(show)
            return scn.placeImage(new Text(this.name(), 40, "black"), 280, 500);
        return scn;
    }
    /** Pause the Sorting */
    public void onKey(String key){
        if(key.equals("p"))
            this.paused = !this.paused;
        if(key.equals("r"))
            init(this.alist.size());
        if(key.equals("s"))
            this.show = !this.show;
        if(key.equals("v"))
            this.vis = !this.vis;
    }
    /** OnTick Do a Step */
    public void onTick(){
        if(!this.paused && !this.done)
            this.sortStep();
    }
    /** Sort one Step */
    void sortStep(){ }
    /** Reinitialize the List with 35 elements */
    void init(){ init(35); }
    /** Reinitialize the List */
    void init(int size){
        this.done = false;
        this.current = -1;
        Random rand = new Random();
        this.alist.clear();
        for(int i = 0; i < size; i++)
            this.alist.add(rand.nextInt(54));
    }
    /** Swap elements at position(s) i/j */
    void swap(int i, int j){
        alist.set(i, alist.set(j, alist.get(i)));
    }

    boolean vis = true;
}

/** Demonstration of Sort */
class SortWorld extends VoidWorld{
    ArrayList<ShowSort> sorts = new ArrayList<ShowSort>();{
        this.sorts.clear();
        this.sorts.add(new Insertion());
        this.sorts.add(new Selection());
        this.sorts.add(new Merge());
        this.sorts.add(new Quick());
    }
    void rand(){
        Random r = new Random();
        this.sorter = sorts.get(r.nextInt(this.sorts.size()));
        this.sorter.init();
    }

    ShowSort sorter;
    SortWorld(){ rand(); }
    public double tickRate(){ return sorter.tickRate(); }
    public Scene onDraw(){
        return this.sorter.onDraw();
    }
    /** Pause the Sorting */
    public void onKey(String key){
        if(key.equals("\n"))
            this.rand();
        else
            this.sorter.onKey(key);
    }
    /** OnTick Do a Step */
    public void onTick(){
        this.sorter.onTick();
    }
}


/** Examples/Tests */
class LectureExamples{
    LectureExamples(){}

    void testIt(Tester t){
        SortWorld start = new SortWorld();
        VoidWorld end = start.bigBang();
    }
}
