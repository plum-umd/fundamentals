#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")
@(require "../utils.rkt")

@lab-title[4]{Same Story, More Syntax}

@section[#:style 'unnumbered #:tag "lab4:intro"]{Hello, Java}

You'll work in this lab with an ad-hoc partner. Help each other get up
and running with IntelliJ and Java.

Our goal for today's lab is to begin compiling and running Java
programs using IntelliJ.

@section[#:tag "lab4:jdk"]{Java Development Kit}

We need to install the Java Development Kit (JDK) to develop Java
programs. This is platform-dependent, so the process will be slightly
different for each of your machines. Help your partner if you already
have the JDK installed.

First,
@link["https://www.oracle.com/technetwork/java/javase/downloads/index.html"]{download
and install} the proper JDK for your platform (you should use Java SE 11.0.2).


@section[#:tag "lab4:intellij"]{IntelliJ}

We'll use IntelliJ as our editor for Java programs. Download and
install the proper
@link["https://www.jetbrains.com/idea/download"]{IntelliJ Community
Edition} for your platform (Windows/Apple/Linux).

Once you've installed IntelliJ, launch it and try to figure out how to
make a new project with a single class named "Hello". Then add this
code, compile, and run first Java program. Let us know if you run into
any issues.

@verbatim|{
  public class Hello {
    public static void main(String[] args) {
        System.out.println("Hello, world.");
    }
  }
}|


@section[#:tag "lab4:lab1"]{Distances, Speeds, and Times in Java: Lab 1 Redux}

The goal for remainder of today's lab is to re-implement the first lab
(@labref{1}) in Java. If you've already completed lab 1 you're in
luck--the translation from @tt{class/0} should be straightforward.

You should translate tests to Java comments; we'll introduce a
@tt{check-expect} style testing mechanism next week.

@section[#:tag "lab4:submit"]{Submit}

Submit a single Java file that contains all your work from today's lab.

