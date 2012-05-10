#lang scribble/manual
@(require "../unnumbered.rkt"
          "../utils.rkt")

@(define (ittt . args) (italic (apply tt args)))

@title*{Subversion}

In this course we will use @link["http://subversion.tigris.org/"]{Subversion}
for collaboration, version control, and homework submission. This guide will
show you everything you'll need to use Subversion in this course.

@section*{Purpose}

Subversion is a version control system that keeps versions of your files in a
"repository", which we will provide for you on a server. The repository will
enable you to retrieve ("checkout", "update") and store ("commit") your files
from multiple locations, and it will enable us to easily access your code for
grading. It also provides a backup mechanism for you in case you ever need to
revert to older versions of your code.

@section*{Getting Subversion}

The Subversion client may be downloaded at:

@indented[
  @url{http://subversion.tigris.org}
]

Binaries for several systems such as Windows, Mac OS X, and Linux are
available.  If you have Windows, for example, you should the follow
the link to ``Windows binaries''.  If you have Linux or Mac OS X,
Subversion is likely pre-installed.

You can confirm your Subversion installation
by running it at your system's command prompt:

@indented[@verbatim{
  $ svn help
  usage: svn <subcommand> [options] [args]
  Subversion command-line client, version 1.6.5.
  ...
}]

@section*[#:tag "svnlayout"]{Your Subversion repository and homework directories}

You will use Subversion to (1) keep track of revisions as you work on your
assignments, and (2) submit your assignments for grading. If you are in pair
number @emph{P}, then your repository is located at

@indented[
  @tt{https://trac.ccs.neu.edu/svn/cs2510summer2012/pairP/}
]

@section*{Subversion Set-up}

To make things easier on the graders this semester, we will be
enforcing a strict directory structure in your Subversion (aka svn)
repository. The layout in Subversion
will look something like this:

@verbatim|{
pairNN
  |--assign1
  |  |--foo.java
  |  |--blah.java
  |--assign2
  .  |--bar.java
  .  .
  .  .
     .
}|

The important thing to note is that you only commit Java source code.
No Eclipse workspaces, no class files, no jar files, etc.

There's an easy way to check what you actually have in Subversion:
just open your repository URL in a browser. That will show you the
exact layout the graders will see when they're looking at your
code. (Note: some browsers seem to have trouble with this. Try this on
Google Chrome if you run into issues).

You only need to create the above structure in Subversion once, but
you will need to check out a new working copy of the repository on
each different machine you work on (with the exception of CCIS
machines that use your same home directory).

@section*{Windows Instructions}

Open the file explorer and navigate to the Z: drive. If you're not on
a CCIS machine (and therefore don't have a Z: drive), just navigate to
your home folder. Right-click on a blank area in the folder and select
"SVN Checkout...". This will bring up the checkout dialog. Here you
need to enter your repository URL (where the repository lives) and the
checkout directory (where your local copy of the repository will be
stored). For the repository directory, type:
https://trac.ccs.neu.edu/svn/cs2510summer2012/pairXYZ

where "XYZ" is your pair number. For the checkout directory, enter
"Z:\classes" (the folder doesn't have to be called "classes", but this
matches what we did in the lab). Click OK. You may have to enter your
CCIS username and password.

The classes folder will be your working copy of your svn
repository. You can think of this folder as mapping directly to your
"pairXYZ" folder in svn - everything that's under pairXYZ in svn will
be checked out under this folder.

@;{Now, open up the classes folder and create a new folder inside it
called "EclipseWorkspace" (all one word). This will be your Eclipse
workspace, where Eclipse stores all of your projects throughout the
semester. Right-click the folder and select "Tortoise SVN" >
"Add...". Make sure the folder is checked and hit OK. Right-click the
folder again and select "SVN Commit...". Type in a commit message like
"Added Eclipse workspace" and click OK.}

Finally, to change your Eclipse workspace to point at this new folder,
see the instructions @seclink["Eclipse"]{below}.

@section*{Command Line Instructions (for Mac and Linux)}

Open up a terminal (On Macs, it's typically under Applications >
Utilities). This should start you at your home directory, but if not,
just type "cd" and hit enter. Then, to check out a working copy of the
repository, type the following:

@verbatim{svn co https://trac.ccs.neu.edu/svn/cs2510summer2012/pairXYZ classes}

where "XYZ" is your pair number. This will create a directory called
"classes" to act as your working copy of your svn repository. You
don't have to call it classes, but that's the name we used in the
lab. You can think of this folder as mapping directly to your
"pairXYZ" folder in svn - everything that's under pairXYZ in svn will
be checked out under this folder.

@section*{More Information}

For more help on Eclipse and Subversion, check out their websites:

@link["http://subversion.apache.org/"]{Subversion}

@link["http://www.eclipse.org/"]{Eclipse}

As always, your tutors and TAs are available for further assistance.