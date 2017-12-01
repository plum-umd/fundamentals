import tester.Tester;

// to represent a node in a DAG of data
abstract class Node<T>{
  T data;
  
  Node(T data){
    this.data = data;
  }
  
  // count the paths to the node with the given data
  abstract public int countPaths(T data);
  
  // find the length of the longest path from this node
  abstract public int maxLength();
  
  // count all possible paths from this node to any terminal
  abstract public int countAllPaths();

  
  // compute the length of the path to the node with the given data
  abstract public int distanceTo(T data);
}

class Terminal<T> extends Node<T>{
  
  Terminal(T data){
    super(data);
  }
  
  // count the paths to the node with the given data
  public int countPaths(T data){
    if (this.data.equals(data))
      return 1;
    else
      return 0;
  }
  
  // find the length of the longest path from this node
  public int maxLength(){
    return 1;
  }
  
  // count all possible paths from this node to any terminal
  public int countAllPaths(){
    return 1;
  }
  
  // compute the length of the path to the node with the given data
  public int distanceTo(T data){
    if (this.data.equals(data))
      return 1;
    else
      return 1000; // nonsense value - we will never have 1000 nodes
  }
  
}

class Pass<T> extends Node<T>{
  Node<T> next;
  
  Pass(T data, Node<T> next){
    super(data);
    this.next = next;
  }
  
  // count the paths to the node with the given data
  public int countPaths(T data){
    if (this.data.equals(data))
      return 1;
    else
      return this.next.countPaths(data);
  }
  
  // find the length of the longest path from this node
  public int maxLength(){
    return 1 + this.next.maxLength();
  }
  
  // count all possible paths from this node to any terminal
  public int countAllPaths(){
    return this.next.countAllPaths();
  }
  
  // compute the length of the path to the node with the given data
  public int distanceTo(T data){
    if (this.data.equals(data))
      return 1;
    else
      return 1 + this.next.distanceTo(data);
  }
}

class Split<T> extends Node<T>{
  Node<T> left;
  Node<T> right;
  
  Split(T data, Node<T> left, Node<T> right){
    super(data);
    this.left = left;
    this.right = right;
  }
  
  // count the paths to the node with the given data
  public int countPaths(T data){
    if (this.data.equals(data))
      return 1;
    else
      return this.left.countPaths(data) + this.right.countPaths(data);
  }
  
  // find the length of the longest path from this node
  public int maxLength(){
    return 1 + Math.max(this.left.maxLength(), this.right.maxLength());
  }
  
  // count all possible paths from this node to any terminal
  public int countAllPaths(){
    return this.left.countAllPaths() + this.right.countAllPaths();
  }
  
  // compute the length of the path to the node with the given data
  public int distanceTo(T data){
    if (this.data.equals(data))
      return 1;
    else
      return 1 + Math.min(this.left.distanceTo(data), 
                          this.right.distanceTo(data));
  }
}

class ExamplesNode{
  ExamplesNode(){}
  
  /*
        kalispera
        /       \
    servus   guten Tag
             /       \
          ciao    dosvidania
           |       /       \
          ahoy  sayonara  adieu
          /  \
       hello bye  
  */
  
  Terminal<String> hello = new Terminal<String>("hello");
  Terminal<String> bye = new Terminal<String>("bye");
  Terminal<String> adieu = new Terminal<String>("adieu");
  Terminal<String> sayonara = new Terminal<String>("sayonara");
  Split<String> ahoy = new Split<String>("ahoy", this.hello, this.bye);
  Pass<String> ciao = new Pass<String>("ciao", this.ahoy);
  Terminal<String> servus = new Terminal<String>("servus");
  Split<String> dosvidania = 
      new Split<String>("dosvidania", this.sayonara, this.adieu);
  Split<String> gutenTag = 
      new Split<String>("guten Tag", this.ciao, this.dosvidania);
  Split<String> kalispera = 
      new Split<String>("kalispera", this.servus, this.gutenTag);

  
  /*
       k
     /   \
    s     g
  /  \  /   \
 t     c      d     
     /  \     |   
    z     \   j
           \  |
             a
            / \
           h   b
  */ 
  Terminal<String> h = new Terminal<String>("h");
  Terminal<String> b = new Terminal<String>("b");
  Split<String> a = new Split<String>("a", this.h, this.b);
  Pass<String> j = new Pass<String>("j", this.a);
  Pass<String> d = new Pass<String>("d", this.j);
  Terminal<String> z = new Terminal<String>("z");
  Split<String> c = new Split<String>("c", this.z, this.a);
  Terminal<String> t = new Terminal<String>("t");
  Split<String> s = new Split<String>("s", this.t, this.c);
  Split<String> g = new Split<String>("g", this.c, this.d);
  Split<String> k = new Split<String>("k", this.s, this.g);
  
  
  // test the method countPaths
  void testCountPaths(Tester t){
    t.checkExpect(this.k.countPaths("h"), 3);
    t.checkExpect(this.kalispera.countPaths("hello"), 1);
    t.checkExpect(this.gutenTag.countPaths("servus"), 0);
  }
  
  // test the method maxLength
  void testMaxLength(Tester t){
    t.checkExpect(this.k.maxLength(), 6);
    t.checkExpect(this.kalispera.maxLength(), 5);
    t.checkExpect(this.gutenTag.maxLength(), 4);
    t.checkExpect(this.g.maxLength(), 5);
  }
  
  // test the method countAllPaths
  void testCountAllPaths(Tester t){
    t.checkExpect(this.k.countAllPaths(), 9);
    t.checkExpect(this.kalispera.countAllPaths(), 5);
    t.checkExpect(this.gutenTag.countAllPaths(), 4);
    t.checkExpect(this.g.countAllPaths(), 5);
  }
  
  // test the method distanceTo
  void testDistanceTo(Tester t){
    t.checkExpect(this.k.distanceTo("h"), 5);
    t.checkExpect(this.kalispera.distanceTo("hello"), 5);
    t.checkExpect(this.gutenTag.distanceTo("servus"), 1002);
  }
}

  