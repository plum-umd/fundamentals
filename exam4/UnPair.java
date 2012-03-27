import tester.*;

class UnPair<X> {
  X left;
  X right;
  UnPair(X left, X right) {
    this.left = left;
    this.right = right;
  }

  public int hashCode() {
    return left.hashCode();
  }

  public boolean same(UnPair<X> p) {
     return (this.left.equals(p.left) &&
             this.right.equals(p.right))
         || (this.left.equals(p.right) &&
             this.right.equals(p.left));
  }

  public boolean equals(Object that) {
    return (that instanceof UnPair)
        && this.same((UnPair<X>)that);
  }
}

class Examples {
    void testUnPair(Tester t) {
	t.checkExpect(new UnPair<Integer>(5,7).equals(new UnPair<Integer>(5,7)),
		      true);
	t.checkExpect(new UnPair<Integer>(5,7).equals(new UnPair<Integer>(7,5)),
		      true);
	t.checkExpect(new UnPair<Integer>(5,7).equals(new UnPair<Integer>(7,7)),
		      false);
    }
}