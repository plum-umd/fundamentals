// Lecture Notes
// CS U2510 Spring 2012
// BookstoreAbstract.java

/*
  A Book is one of
    -- PrintBook
    -- AudioBook
    -- E-Book
  
  A PrintBook is (make-print-book String String Num)
  (define-struct print-book (title author price))

  An AudioBook is (make-audio-book String String Num Num)
  (define-struct audio-book (title author price no-cds))

  An E-Book is (make-online-book String String Num String)
  (define-struct e-book (title author price format))

;; Examples of books
(define oms (make-audio-book "Old Man and the Sea" "EH" 10, 2))
(define eos (make-e-book "Elements of Style" "EBW" 20, "eos.net"))
(define htdp (make-print-book "HtDP" "MF" 60))
*/

import tester.*;


/*
                      +-------+
                      | IBook |
                      +-------+
                         / \
                         ---
                          |
                   +---------------+
                   | ABook         |
                   +---------------+
                   | String title  |
                   | String author |
                   | int price     |
                   +---------------+
                         / \
                         ---
                          |
         -------------------------------------------
         |                    |                    |
 +---------------+    +---------------+    +---------------+
 | PrintBook     |    | AudioBook     |    | EBook         |
 +---------------+    +---------------+    +---------------+
 +---------------+    | int noCDs     |    | String format |
                      +---------------+    +---------------+
*/

// to represent various books in a bookstore
interface IBook{
  
/*  
 the sale price of the book depends on the daily discounts
 these may differ depending on the kind of book
 suppose today we have the following discounts:
 there is 30% discount on print books
 there is 20% discount on e-books
 online books sell at full price
*/

  // was this book written by the given author?
  public boolean writtenBy(String authorName);
  
  // compute the discounted sale price for this book
  public int salePrice();
  
  // does this book sell for less than the given book?
  public boolean isCheaperThan(IBook that);
  
}


abstract class ABook implements IBook{
  String title;
  String author;
  int price;
  
  ABook(String title, String author, int price){
    this.title = title;
    this.author = author;
    this.price = price;  
  }
  
  /* TEMPLATE:
  FIELDS
   ... this.title ...       -- String
   ... this.author ...      -- String
   ... this.price ...       -- int
 
  METHODS
   ... this.salePrice() ...       -- int
   ... this.writtenBy(String) ... -- boolean
   ... this.sameAuthor(ABook) ... -- boolean
*/  
  
  // was this book written by the given author?
  public boolean writtenBy(String authorName){
    return this.author.equals(authorName);
  }
  
  // compute the discounted sale price for this online book
  public abstract int salePrice();
  
  // does this book sell for less than the given book?
  public boolean isCheaperThan(IBook that){
    return this.salePrice() < that.salePrice();
  }
  
  //is the author if this book the same as the author of the
  //given book
  public boolean sameAuthor(ABook that){
    return this.author.equals(that.author);
  }
}

// to represent a printed book in a bookstore
class PrintBook extends ABook{
  
  PrintBook(String title, String author, int price){
    super(title, author, price);  
  }
  

/* TEMPLATE:
   FIELDS
    ... this.title ...       -- String
    ... this.author ...      -- String
    ... this.price ...       -- int
  
   METHODS
    ... this.salePrice() ...       -- int
    ... this.writtenBy(String) ... -- boolean
    ... this.sameAuthor(ABook) ... -- boolean
*/  
  
  // compute the discounted sale price for this printed book
  public int salePrice(){
    return this.price - 3 * this.price / 10;
  }
}


// to represent an audio book in a bookstore
class AudioBook extends ABook{
  int noCDs;
  
  AudioBook(String title, String author, int price, int noCDs){
    super(title, author, price); 
    this.noCDs = noCDs; 
  }

/* TEMPLATE:
   FIELDS
    ... this.title ...       -- String
    ... this.author ...      -- String
    ... this.price ...       -- int
    ... this.noCDs ...       -- int
  
   METHODS
    ... this.salePrice() ...       -- int
    ... this.writtenBy(String) ... -- boolean
    ... this.sameAuthor(ABook) ... -- boolean
*/  
  // compute the discounted sale price for this online book
  public int salePrice(){
    return this.price - 2 * this.price / 10;
  }
}

// to represent an e-book in a bookstore
class EBook extends ABook{
  String format;

  EBook(String title, String author, int price, String format){
    super(title, author, price); 
    this.format = format;    
  }
  

/* TEMPLATE:
   FIELDS
    ... this.title ...       -- String
    ... this.author ...      -- String
    ... this.price ...       -- int
    ... format ...           -- int
  
   METHODS
    ... this.salePrice() ...       -- int
    ... this.writtenBy(String) ... -- boolean
    ... this.sameAuthor(ABook) ... -- boolean
*/  
  // compute the discounted sale price for this e-book
  public int salePrice(){
    return this.price;
  }
}

//Examples and tests for the classes that implement the IBook interface
class ExamplesBooks3{
  ExamplesBooks3(){}
  
  ABook oms = new PrintBook("Old Man and the Sea", "EH", 20);
  ABook eos = new PrintBook("Elements of Style", "EBW", 20);
  ABook htdp = new PrintBook("HtDP", "MF", 60);
  
  ABook omsAudio = new AudioBook("Old Man and the Sea", "EH", 10, 2);
  ABook snAudio = new AudioBook("Shipping News", "AP", 10, 2);
  
  ABook omsEBook = new EBook("Old Man and the Sea", "EH", 10, "url1");
  ABook htdpEBook = new EBook("HtDP", "MF", 60, "htdp.org");
  
  
  // test the method writtenBy in the class Book
  boolean testWrittenBy(Tester t){
  return
    t.checkExpect(this.oms.writtenBy("EH"), true) &&
    t.checkExpect(this.oms.writtenBy("MF"), false) &&
    t.checkExpect(this.omsAudio.writtenBy("EH"), true) &&
    t.checkExpect(this.omsAudio.writtenBy("EBW"), false) &&
    t.checkExpect(this.omsEBook.writtenBy("EH"), true) &&
    t.checkExpect(this.htdpEBook.writtenBy("EBW"), false);
  }
  
  // test the method salePrice in the class Book
  boolean testSalePrice(Tester t){
  return
    t.checkExpect(this.oms.salePrice(), 14) &&
    t.checkExpect(this.omsAudio.salePrice(), 8) &&
    t.checkExpect(this.omsEBook.salePrice(), 10) &&
    t.checkExpect(this.htdpEBook.salePrice(), 60);
  }
  
  // test the method isCheaperThan in the class IBook
  boolean testIsCheaperThan(Tester t){
  return
    t.checkExpect(this.oms.isCheaperThan(this.omsAudio), false) &&
    t.checkExpect(this.omsAudio.isCheaperThan(this.oms), true) &&
    t.checkExpect(this.omsEBook.isCheaperThan(this.oms), true) &&
    t.checkExpect(this.htdpEBook.isCheaperThan(this.omsAudio), false);
  }
  
  boolean testSameAuthor(Tester t){
    return
    t.checkExpect(this.oms.sameAuthor(this.htdp), false) &&
    t.checkExpect(this.oms.sameAuthor(this.omsEBook), true) &&
    t.checkExpect(this.oms.sameAuthor(this.omsAudio), true) &&
    t.checkExpect(this.omsAudio.sameAuthor(this.oms), true) &&
    t.checkExpect(this.omsAudio.sameAuthor(this.omsEBook), true) &&
    t.checkExpect(this.omsAudio.sameAuthor(this.snAudio), false) &&
    t.checkExpect(this.omsEBook.sameAuthor(this.eos), false) &&
    t.checkExpect(this.omsEBook.sameAuthor(this.omsAudio), true) &&
    t.checkExpect(this.omsEBook.sameAuthor(this.htdpEBook), false);
  }

}