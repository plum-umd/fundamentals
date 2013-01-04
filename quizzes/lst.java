// A LoN is one of 
// - new Cons(Number, LoN)
// - new Mt()
// and implements LoN
interface LoN {
  // get the element at the given index
  Integer listRef(Integer index);
}

class Cons implements LoN {
  Integer f;
  LoN r;
  Cons(Integer f, LoN r) {
    this.f = f;
    this.r = r;
  }
  public Integer listRef(Integer index) {
    return (index == 0) ?
           this.f : this.r.listRef(index);
  }
}

class Mt implements LoN {
  Mt() {}
  public Integer listRef(Integer index) {
    throw new RuntimeException("Index too large");
  }
}
