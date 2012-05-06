// CS 2510 Spring 2012
// Lab 4

import tester.*;

// to represent different files in a computer
interface IFile{
  
  // compute the size of this file
  int size();
  
  // compute the time (in seconds) to download this file
  // at the given download rate
  int downloadTime(int rate);
  
  // is the owner of this file the same 
  // as the owner of the given file?
  boolean sameOwner(IFile that);
}

// to represent a text file
class TextFile implements IFile{
  String name;
  String owner;
  int length;   // in bytes
  
  TextFile(String name, String owner, int length){
    this.name = name;
    this.owner = owner;
    this.length = length;
  }
  
  // compute the size of this file
  int size(){
    return this.length;
  }  
  
  // compute the time (in seconds) to download this file
  // at the given download rate
  int downloadTime(int rate){
    return 0;
  }
  
  // is the owner of this file the same 
  // as the owner of the given file?
  boolean sameOwner(IFile that){
    return true;
  }
}

//to represent an image file
class ImageFile implements IFile{
  String name;
  String owner;
  int width;   // in pixels
  int height;  // in pixels
  
  ImageFile(String name, String owner, int width, int height){
    this.name = name;
    this.owner = owner;
    this.width = width;
    this.height = height;
  }
  
  // compute the size of this file
  int size(){
    return this.width * this.height;
  }  
  
  // compute the time (in seconds) to download this file
  // at the given download rate
  int downloadTime(int rate){
    return 0;
  }
  
  // is the owner of this file the same 
  // as the owner of the given file?
  boolean sameOwner(IFile that){
    return true;
  }
}


//to represent an audio file
class AudioFile implements IFile{
  String name;
  String owner;
  int speed;   // in bytes per second
  int length;  // in seconds of recording time
    
  AudioFile(String name, String owner, int speed, int length){
    this.name = name;
    this.owner = owner;
    this.speed = speed;
    this.length = length;
  }
  
  // compute the size of this file
  int size(){
    return this.speed * this.length;
  }  
  
  // compute the time (in seconds) to download this file
  // at the given download rate
  int downloadTime(int rate){
    return 0;
  }
  
  // is the owner of this file the same 
  // as the owner of the given file?
  boolean sameOwner(IFile that){
    return true;
  }
}

class ExamplesFiles{
  ExamplesFiles(){}
  
  IFile text1 = new TextFile("English paper", "Maria", 1234);
  IFile picture = new ImageFile("Beach", "Maria", 400, 200);
  IFile song = new AudioFile("Help", "Pat", 200, 120);
  
  // test the method size for the classes that represent files
  boolean testSize(Tester t){
    return
    t.checkExpect(this.text1.size(), 1234) &&
    t.checkExpect(this.picture.size(), 80000) &&
    t.checkExpect(this.song.size(), 24000);
  }
}