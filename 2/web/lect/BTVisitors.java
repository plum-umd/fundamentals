interface BT<X> {
    <R> R accept(BTVisitor<X,R> visitor);
}

class Leaf<X> implements BT<X> {
    X val;
    Leaf(X val) {
	this.val = val;
    }
    public <R> R accept(BTVisitor<X,R> visitor) {
	return visitor.visitLeaf(this.val);
    }
}

class Node<X> implements BT<X> {
    X val;
    BT<X> left;
    BT<X> right;
    Node(X val, BT<X> left, BT<X> right) {
	this.val = val;
	this.left = left;
	this.right = right;
    }
    public <R> R accept(BTVisitor<X,R> visitor) {
	return visitor.visitNode(this.val, this.left, this.right);
    }
}

interface BTVisitor<X,R> {
    R visitLeaf(X val);
    R visitNode(X val, BT<X> left, BT<X> right);
}

class Sum implements BTVisitor<Integer, Integer> {
    public Integer visitLeaf(Integer val) {
	return val;
    }

    public Integer visitNode(Integer val, BT<Integer> left, BT<Integer> right) {
	return val + left.accept(this) + right.accept(this);
    }
}

class Height<X> implements BTVisitor<X, Integer> {
    public Integer visitLeaf(X val) {
	return 1;
    }
    public Integer visitNode(X val, BT<X> left, BT<X> right) {
	return 1 + Math.max(left.accept(this), right.accept(this));
    }
}

class Concat implements BTVisitor<String, String> {
    public String visitLeaf(String val) {
	return val;
    }
    public String visitNode(String val, BT<String> left, BT<String> right) {
	return val.concat(left.accept(this).concat(right.accept(this)));
    }
}



