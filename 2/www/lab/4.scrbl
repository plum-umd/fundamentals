#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")
@(require "../utils.rkt")

@lab-title[4]{Same Story, More Syntax}

@section[#:style 'unnumbered #:tag "lab4:intro"]{Hello, Java}

You'll work in this lab with your
@link["https://piazza.com/class/jcspfhewmdn41y?cid=27"]{assigned
partner}. Help each other get up and running with IntelliJ and Java.

Our goal for today's lab is to begin compiling and running Java
programs using IntelliJ.

@section[#:tag "lab4:jdk"]{Java Development Kit}

We need to install the Java Development Kit (JDK) to develop Java
programs. This is platform-dependent, so the process will be slightly
different for each of your machines. Help your partner if you already
have the JDK installed.

First,
@link["http://www.oracle.com/technetwork/java/javase/downloads/jdk9-downloads-3848520.html"]{download
and install} the proper JDK for your platform.

Next you need to configure the @tt{JAVA_HOME} environment variable for
Java programs. Programs look for the standard Java libraries in this
location, so it will be installation-specific.

Set @tt{JAVA_HOME} to the directory where you installed the JDK,
e.g. @tt{/usr/lib/jvm/jdkX.X.X_XX}, @tt{C:\Program
Files\Java\jdkX.X.X_XX}.

In a terminal (Mac/Linux):

@tt{    > export JAVA_HOME=/path/to/jdkX.X.X_XX}

In Windows, configure this under for "System > Advanced > Environment
Variables", and add the variable @tt{JAVA_HOME} with the proper
directory as its value.

In Linux, you will also have to extend the @tt{PATH} environment
variable to include the directory @tt{$JAVA_HOME/bin}. See
@link["https://www.java.com/en/download/help/path.xml"]{here} for
details if this applies to you.


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
