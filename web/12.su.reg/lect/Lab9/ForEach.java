import tester.Traversal;

/** Encapsulates a function that updates an accumulator of type
 *    <code>R</code> given an element of a collection of type
 *    <code>T</code> */
interface ITR2R<T, R>{
    R apply(T t, R s);
}

/** An update method function class for implement OrMap with
 *    a given Balloon selector */
class OrSelectUpdater implements ITR2R<Balloon, Boolean>{
    ISelect<Balloon> pick;

    OrSelectUpdater(ISelect<Balloon> pick){
        this.pick = pick;
    }

    public Boolean apply(Balloon b, Boolean acc){
        return this.pick.select(b) || acc;
    }
}

/** Encapsulate three different implementations of loops over
 *    a data using a functional <code>Traversal</code>
 *    <i>iterator</i> in accumulator style using the given 
 *    {@link ITR2R} function-object.
 */
public class ForEach<T, S>{
    S acc;
    ITR2R<T, S> update;
    Traversal<T> tr;

    ForEach(S acc, ITR2R<T, S> update, Traversal<T> tr){
        this.acc = acc;
        this.update = update;
        this.tr = tr;
    }

    /** Loop implemented using recursion, without mutation */
    S loopRec(){
        if(this.tr.isEmpty()){
            return this.acc;
        }else{
            return new ForEach<T, S>(this.update.apply(this.tr.getFirst(), this.acc),
                                     this.update, this.tr.getRest())
                          .loopRec();
        }
    }
    /** Loop implemented using a While loop and mutation */
    S loopWhile(){
        while (!this.tr.isEmpty()){
            this.acc = this.update.apply(this.tr.getFirst(), this.acc);
            this.tr = this.tr.getRest();
        }
        return this.acc;
    }

    /** Loop implemented using a For loop and mutation */
    S loopFor(){
        for (; !this.tr.isEmpty(); this.tr = this.tr.getRest()){
            this.acc = this.update.apply(this.tr.getFirst(), this.acc);     
        }
        return this.acc;
    }
}
