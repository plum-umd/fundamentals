import tester.*;

interface Tree {
    //> 1pt for purpose
    // count the height of this tree
    Integer height();
    <T> T visit(TreeVisitor<T> v);    
}
interface TwoTree extends Tree {}
interface ThreeTree extends Tree {}
interface TreeVisitor<T> {
  T visitLeaf(String s);
  T visitTwoNode(ThreeTree l, ThreeTree r);
  T visitThreeNode(TwoTree l, TwoTree m, TwoTree r);
}

//> 4pts for class definitions
//> 3pts for method definitions
class Leaf implements TwoTree, ThreeTree {
    String val;
    Leaf(String v) { val = v; }

    public <T> T visit(TreeVisitor<T> v) { 
	return v.visitLeaf(this.val);
    }

    public Integer height() { return 1; }
}

class TwoNode implements TwoTree {
    ThreeTree left;
    ThreeTree right;

    TwoNode(ThreeTree l, ThreeTree r) { left = l; right = r; }

    public <T> T visit(TreeVisitor<T> v) { 
	return v.visitTwoNode(this.left, this.right);
    }

    public Integer height() { return Math.max(this.left.height(), this.right.height()); }
}

class ThreeNode implements ThreeTree {
    TwoTree left;
    TwoTree mid;
    TwoTree right;

    ThreeNode(TwoTree l, TwoTree m, TwoTree r) { left = l; right = r; mid = m; }

    public <T> T visit(TreeVisitor<T> v) { 
	return v.visitThreeNode(this.left, this.mid, this.right);
    }
    public Integer height() { return Math.max(this.left.height(),
					      Math.max(this.mid.height(),
						       this.right.height())); }
}

class LongestVisitor implements TreeVisitor<String> {
    public String visitLeaf(String s) { return s; }
    public String visitTwoNode(ThreeTree l, ThreeTree r) { 
	String s1 = l.visit(this);
	String s2 = r.visit(this);
	return (s1.length() > s2.length())?s1:s2;
    }
    public String visitThreeNode(TwoTree l, TwoTree m, TwoTree r) { 
	String s1 = l.visit(this);
	String s2 = r.visit(this);
	String s3 = m.visit(this);
	if (s1.length() > s2.length() && s1.length() > s3.length())
	    return s1;
	else if (s2.length() > s3.length())
	    return s2;
	else
	    return  s3;
    }
}


class CountVisitor implements TreeVisitor<Integer> {
    public Integer visitLeaf(String s) { return 1; }
    public Integer visitTwoNode(ThreeTree l, ThreeTree r) { return l.visit(this) + r.visit(this); }
    public Integer visitThreeNode(TwoTree l, TwoTree m, TwoTree r) { 
	return l.visit(this) + r.visit(this) + m.visit(this);
    }
}

class Examples {

    void test(Tester t) {
	ThreeTree r = new Leaf("Rhinocerous");
	TwoTree h = new Leaf("Hat");
	TwoTree b = new Leaf("Bread");
	ThreeTree t1 = new ThreeNode(new Leaf("Zebra"),new Leaf("Dog"),new Leaf("Cat"));
	TwoTree t2 = new TwoNode(t1,r);
	Tree t3 = new ThreeNode(b,h,t2);

	t.checkExpect(t3.height(), 4);
	t.checkExpect(r.height(), 1);
	t.checkExpect(t2.height(), 3);

	t.checkExpect(r.visit(new CountVisitor()), 1);
	t.checkExpect(t2.visit(new CountVisitor()), 4);
	t.checkExpect(t3.visit(new CountVisitor()), 6);
	t.checkExpect(h.visit(new LongestVisitor()), "Hat");
	t.checkExpect(t2.visit(new LongestVisitor()), "Rhinocerous");
	t.checkExpect(t3.visit(new LongestVisitor()), "Rhinocerous");
	
    }
}