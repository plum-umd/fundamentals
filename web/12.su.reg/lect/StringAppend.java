public class StringAppend implements IVisitorBT<String,String> {
    public String visitLeaf() { return ""; }
    public String visitNode(String val, IBT<String> left, IBT<String> right) {
	return val + left.accept(this) + right.accept(this);
    }
}