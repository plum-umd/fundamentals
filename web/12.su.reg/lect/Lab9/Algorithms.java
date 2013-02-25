import java.util.ArrayList;
import tester.*;

/** Implements two traversal-based algorithms (contains and countSuch) and 
 *    discusses templates for different versions of Loop based methods.
 */
public class Algorithms{

    /** Determine if any data item from the given Traversal satisfies
     *    the given predicate. */
    public <T> boolean contains(Traversal<T> tr, ISelect<T> choice){
        if(tr.isEmpty()){
            return false;
        }else{    
            return (choice.select(tr.getFirst()) ||
                    contains(tr.getRest(), choice));
        }
    }

    /** Count how many data elements from the given Traversal
     *    satisfy the given predicate */
    public <T> int countSuch(Traversal<T> tr, ISelect<T> choice){
        if(tr.isEmpty()){                                  
            return 0;
        }else{    
            if(choice.select(tr.getFirst())){   
                return 1 + countSuch(tr.getRest(), choice); 
            }else{
                return countSuch(tr.getRest(), choice);
            }
        }
    }


    /**********************************************************
    // RECURSION: TEMPLATE ANALYSIS
    //*********************************************************

    public <T> ReturnType methodName(Traversal<T> tr){
       //                       +--------------------+
       // Invoke the methodAcc: | acc <-- BASE-VALUE |
       //                       +--------------------+
       return methodNameAcc(tr, BASE-VALUE);
    }

    public <T> ReturnType methodNameAcc(Traversal<T> tr, ReturnType acc){

      ... tr.isEmpty()) ...             -- boolean      :: PREDICATE
      if true:
        ... acc ...                     -- ReturnType   :: BASE-VALUE
      else:
           +---------------+  
        ...| tr.getFirst() | ...        -- T            :: CURRENT-ELEMENT
           +---------------+  

           +-----------------------+
       ... | update(T, ReturnType) |    -- ReturnType   :: UPDATE
           +-----------------------+         
       e.g.: update(tr.getFirst(), acc)

           +--------------+  
       ... | tr.getRest() | ...         -- Traversal<T> :: ADVANCE
           +--------------+
       e.g.: methodNameAcc(tr.getRest(), update(tr.getFirst(), acc))
    }


    // COMPLETED LOOP METHOD(S) TEMPLATE:
    //*********************************************************
    public <T> ReturnType methodName(Traversal<T> tr){
      //                        +--------------------+
      // Invoke the Acc method: | acc <-- BASE-VALUE |
      //                        +--------------------+
      return methodNameAcc(tr, BASE-VALUE);
    }

    public <T> ReturnType methodName(Traversal<T> tr, ReturnType acc){
         +---predicate--+
      if(| tr.isEmpty() |) 
         +--------------+
          return acc;
      else
                               +----advance---+  +----update-using-current----+       
          return methodNameAcc(| tr.getRest() |, | update(tr.getFirst(), acc) |);
                               +--------------+  +----------------------------+
    }

    <T> ReturnType update(T t, ReturnType acc){
       ... Update Accumulator ...
    }   
    **************************************************************/


    /**************************************************************
    // OR-MAP:
       // ACC-METHOD HEADER:
       <T> boolean orMapAcc(Traversal<T> tr, boolean acc, ISelect<T> choice)
       // BASE-VALUE:
       false
       // UPDATE:
       <T> boolean update(T t, boolean acc, ISelect<T> choice){
          return (choice.select(t)) || acc;
       }
    **************************************************************/

    //** RECURSIVE VERSION    
    /** Determine if any data item from the given Traversal satisfies
     *    the given predicate. */
    public <T> boolean orMapRec(Traversal<T> tr, ISelect<T> choice){
        return orMapAcc(tr, false, choice);
    }

    /** Accumulator Helper */
    <T> boolean orMapAcc(Traversal<T> tr, boolean acc, ISelect<T> choice){
        if(tr.isEmpty())
            return acc;
        else
            return orMapAcc(tr.getRest(), 
                    updateOrMap(tr.getFirst(), acc, choice),
                    choice);
    }

    /** Produce the updated value of the accumulator
     * @param t the current element used in the update
     * @param acc the current value of the accumulator
     * @param choice the given predicate.
     * @return the updated value of the accumulator.
     */
    <T> boolean updateOrMap(T t, boolean acc, ISelect<T> choice){
        return (choice.select(t) || acc);
    }

    /**************************************************************
    // COUNT-SUCH:
       //** ACC-METHOD HEADER:
       <T> int countSuchAcc(Traversal tr, int acc, ISelect choice)
       //** BASE-VALUE:
       0
       //** UPDATE:
       <T> int update(T t, int acc, ISelect choice){
          if(choice.select(t)) 
             return acc + 1;
          else
             return acc;
       }
    **************************************************************/

    //** RECURSIVE VERSION */
    /** Count how many data elements from the given Traversal
     *    satisfy the given predicate */
    public <T> int countSuchRec(Traversal<T> tr, ISelect<T> choice){
        return countSuchAcc(tr, 0, choice);
    }

    /** Accumulator Helper */
    <T> int countSuchAcc(Traversal<T> tr, int acc, ISelect<T> choice){
        if(tr.isEmpty()) 
            return acc;
        else
            return countSuchAcc(tr.getRest(), 
                    updateCountSuch(tr.getFirst(), acc, choice),
                    choice);
    } 

    /** Produce the updated value of the accumulator
     * @param t the current element used in the update
     * @param acc the current value of the accumulator
     * @param choice the given predicate.
     * @return the updated value of the accumulator.
     */
    <T> int updateCountSuch(T t, int acc, ISelect<T> choice){
        if(choice.select(t))
            return acc + 1;
        else
            return acc;
    }


    /**********************************************************
    // WHILE LOOP: TEMPLATE ANALYSIS
    //*********************************************************

    public <T> ReturnType methodName(Traversal<T> tr){
       ReturnType acc = BASE-VALUE;
       while (CONTINUATION-PREDICATE){
          acc = UPDATE (CURRENT-ELEMENT, acc);
          tr  = ADVANCE;
       }
       return acc;
    }

    // COMPLETE METHOD/WHILE TEMPLATE:
    //************************************************************
    public <T> ReturnType methodName(Traversal<T> tr){
     +------------------------------+
     | ReturnType acc = BASE-VALUE  |;
     +------------------------------+
              +---------------+
       while (| !tr.isEmpty() |){
              +---------------+
               +----------------------------+  
         acc = | update(tr.getFirst(), acc) |;
               +----------------------------+  
              +--------------+
         tr = | tr.getRest() |;
              +--------------+
       }
       return acc;
    }

    <T> ReturnType update(T t, ReturnType acc){
       ... Update Accumulator ...
    }
    **************************************************************/

    //** WHILE-LOOP VERSION */
    /** Determine if any data item from the given Traversal satisfies
     *    the given predicate. */
    public <T> boolean orMapWhile(Traversal<T> tr, ISelect<T> choice){
        // preamble: Define accumulator, initialize it to the BASE-VALUE
        boolean acc = false;

        // loop header: while(continuation-predicate)
        while(!tr.isEmpty()){ 

            // loop body: update
            acc = updateOrMap(tr.getFirst(), acc, choice);

            // loop advance:
            tr = tr.getRest();
        }
        // postmortem: produce the result
        return acc;
    }


    /**********************************************************
    // FOR LOOP: TEMPLATE ANALYSIS
    //*********************************************************
    public <T> ReturnType methodName(Traversal<T> tr){
       ReturnType acc = BASE-VALUE;
       for(INITIALIZATION;
           CONTINUATION-PREDICATE;
           tr  = ADVANCE)
       {
          acc = UPDATE (CURRENT-ELEMENT, acc);
       }
       return acc;
    }

    // COMPLETE METHOD TEMPLATE:
    //***********************************************************
    public <T> ReturnType methodName(Traversal<T> tr){
      +------------------------------+
      | ReturnType acc = BASE-VALUE |;
      +------------------------------+
      for( **No initialization needed **;
        +---------------+
        | !tr.isEmpty() |;
        +---------------+
               +--------------+
          tr = | tr.getRest() |)
               +--------------+
      {
               +----------------------------+  
         acc = | update(tr.getFirst(), acc) |;
               +----------------------------+  
      }
      return acc;
    }

    <T> ReturnType update(T t, ReturnType acc){
       ... Update Accumulator ...
    }
    **************************************************************/
    
    //** FOR-LOOP VERSION */
    /** Determine if any data item from the given Traversal satisfies
     *    the given predicate. */
    public <T> boolean orMapFor(Traversal<T> tr, ISelect<T> choice){

        // Define the accumulator and initialize it to the BASE-VALUE;  
        boolean acc = false;

        // LOOP HEADER: 
        //   for( ... accumulator is already defined and initialized ... ; 
        //        CONTINUATION-PREDICATE; 
        //        ADVANCE)
        for(; !tr.isEmpty();  tr = tr.getRest()){

            // LOOP BODY: uses CURRENT-ELEMENT
            acc = updateOrMap(tr.getFirst(), acc, choice);
        }
        // postmortem: produce the result
        return acc;
    }


    /**********************************************************
    // FOR LOOP/ARRAYLIST: TEMPLATE ANALYSIS
    //*********************************************************
    public <T> ReturnType methodName(ArrayList<T> alist){
       ReturnType acc = BASE-VALUE;

       for(int index = 0;            // define index, start at the beginning
           CONTINUATION-PREDICATE;
           index  = ADVANCE)
       {
          acc = UPDATE (CURRENT-ELEMENT, acc);
       }
       return acc;
    }
 
    // COMPLETE METHOD TEMPLATE:
    //***********************************************************
    public <T> ReturnType methodName(ArrayList<T> alist){
      +------------------------------+
      | ReturnType acc = BASE-VALUE |;
      +------------------------------+
        for(int index = 0;
          +----------------------+
          | index < alist.size() |;
          +----------------------+
                    +-----------+
            index = | index + 1 |)
                    +-----------+
        {
                 +-------------------------------+  
           acc = | update(alist.get(index), acc) |;
                 +-------------------------------+  
        }
        return acc;
     }
 
     <T> ReturnType update(T t, ReturnType acc){
        ... Update Accumulator ...
    }
    **************************************************************/

    //** COUNTED FOR-LOOP VERSION */
    /** Determine if any data item from the given Traversal satisfies
     *    the given predicate. */
    public <T> boolean orMapForCounted(ArrayList<T> alist, ISelect<T> choice){
        // Define the accumulator and initialize it to the BASE-VALUE;  
        boolean acc = false;

        // LOOP HEADER: 
        //   for( ... accumulator is already defined and initialized ... 
        //        ...BUT initialize the loop index: int index = 0; 
        //        CONTINUATION-PREDICATE: index < alist.size() 
        //        ADVANCE: index = index + 1
        for(int index = 0; index < alist.size(); index = index + 1){

            // LOOP BODY: uses current element
            acc = updateOrMap(alist.get(index), acc, choice);
        }
        // postmortem: produce the result
        return acc;
    }
}