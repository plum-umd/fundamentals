// Assignment 4
// CS 2510 Spring 2012
// FilesDirectories.java

/*
             +-----------------------+
             |  +-----------------+  |
             |  |                 |  |
             v  v                 |  |
         +-------+                |  |
         | ILoFD |                |  |
         +-------+                |  |
            / \                   |  | 
            ---                   |  |
             |                    |  |
      ---------------             |  |
      |             |             |  |
  +--------+  +------------+      |  |
  | MtLoFD |  | ConsLoFD   |      |  |
  +--------+  +------------+      |  |
            +-| IFD first  |      |  |
            | | ILoFD rest |------+  |
            | +------------+         |
            |                        |
            v                        |
           +-------+                 |
           | IFD   |                 |
           +-------+                 |
               / \                   |
               ---                   |
                |                    |
        -------------------          |
        |                 |          |
  +-------------+  +--------------+  |
  | File        |  | Directory    |  |
  +-------------+  +--------------+  |
  | String name |  | String name  |  |
  | int size    |  | ILoFD fdlist |--+
  | String kind |  +--------------+  
  +-------------+ 
*/


// to represent a list of files and directories in a computer file system
interface ILoFD{
}

// to represent an empty list of files and directories in a computer file system
class MtLoFD implements ILoFD{
  MtLoFD(){}  
}

// to represent a nonempty list of files and directories in a computer file system
class ConsLoFD implements ILoFD{
  IFD first;
  ILoFD rest;
  
  ConsLoFD(IFD first, ILoFD rest){
    this.first = first;
    this.rest = rest;
  }  

}

// to represent files and directories in a computer file system
interface IFD{
}


// to represent files in a computer file system
class File implements IFD{
  String name;
  int size;
  String kind;
  
  File(String name, int size, String kind){
    this.name = name;
    this.size = size;
    this.kind = kind;
  }
}

// to represent a directory in a computer file system
class Directory implements IFD{
  String name;
  ILoFD fdlist;
  
  Directory(String name, ILoFD fdlist){
    this.name = name;
    this.fdlist = fdlist;
  }

}
  
// Examples and tests for the class hierarchy for a computer file system
class Examples{
  Examples(){}
  
  /*
    Home:  
          Docs: 
                      text1
                      text2
          Pictures: 
                      pic1
                      Archives:   
                                  pic2
                                  pic3
  */
    
  IFD pic1 = new File("party", 6000, "jpeg");
  IFD pic2 = new File("beach", 3500, "jpeg");
  IFD pic3 = new File("river", 2900, "jpeg");
  
  IFD text1 = new File("letter", 600, "txt");
  IFD text2 = new File("memo", 500, "txt");
  
  ILoFD mtlofd = new MtLoFD();
  
  ILoFD archList = 
    new ConsLoFD(this.pic2, new ConsLoFD(this.pic3, this.mtlofd));    
  IFD archives = new Directory("Archives", this.archList);
  
  ILoFD picList = new ConsLoFD(this.pic1,
                    new ConsLoFD(this.archives, this.mtlofd)); 
  IFD pictures = new Directory("Pictures", this.picList);
                   
  ILoFD docList = new ConsLoFD(this.text1, new ConsLoFD(this.text2, this.mtlofd));                                        
  IFD docs = new Directory("Docs", this.docList);
  
  ILoFD homeList = new ConsLoFD(this.docs, new ConsLoFD(this.pictures, this.mtlofd));                                        
  IFD home = new Directory("Home", this.homeList);
}