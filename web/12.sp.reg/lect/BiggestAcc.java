// Find the largest element in the visited tree or val.
public class BiggestAcc implements IVisitorBT<Integer,Integer> {
    Integer val;
    BiggestAcc(Integer val) {
	this.val = val;
    }

    public Integer visitLeaf() { return this.val; }
    public Integer visitNode(Integer val, IBT<Integer> left, IBT<Integer> right) {	
	return Math.max(val,
			Math.max(left.accept(this),
				 right.accept(this)));
    }
}