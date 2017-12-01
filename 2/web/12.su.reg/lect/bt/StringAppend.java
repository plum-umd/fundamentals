class StringAppend implements IVisitorBT<String,String> {
    public String visitNode(String val, 
			    IBT<String> left, 
			    IBT<String> right) {
	return left.accept(this) + val + right.accept(this);
    }

    public String visitLeaf() {
	return "";
    }
}
