#lang scribble/manual
@(require "unnumbered.rkt"
          "utils.rkt")

@(define (ittt . args) (italic (apply tt args)))

@title*{Git}

In this course we will use @link["http://git-scm.com/"]{Git} and
@link["http://github.com/"]{Github} for collaboration, version
control, and homework submission. This guide will show you everything
you'll need to use git in this course.

@section*{Purpose}

Git is a version control system that keeps versions of your files in a
``repository'', which we will provide for you using the Github hosting
service. The repository will enable you to retrieve (``clone'',
``pull''), locally save (``commit''), and share (``push'') your files
from multiple locations, and it will enable us to easily access your
code for grading. It also provides a backup mechanism for you in case
you ever need to revert to older versions of your code.

@section*{Register a Github account}

The first thing you will need is a Github account.  To do this, go to:

@indented[
  @url{http://github.com/}
]

Follow the ``Sign up for free'' instructions.  Please use your CCIS
email address when you register (this important for getting you a free
account with perks that we will need for the course).  If possible, we
suggest re-using your CCIS username as your Github username.

After you've created a github account, you will need to join the
``CCIS-2510H'' organization.  To do that, you should communicate your
github username to the course staff so they can add you to the
organization.

@section*{Get Git}

Github provides clients for both Windows and Macs at the following URLs:

@indented[
  @url{http://windows.github.com/}
]
@indented[
  @url{http://mac.github.com/}
]

Unfortunately neither of these clients are available on the lab
computers.  What you'll use in the lab is TortoiseGit:

@indented[
  @url{http://code.google.com/p/tortoisegit/}
]

You're welcome to use whatever git client that suits your needs.


@section*{Your Github repositories}

You will use git to (1) keep track of revisions as you work on your
assignments, and (2) submit your assignments for grading.

Each pair will form a @emph{team}, and each team will create a
repository for their work.  Within this repository will be a directory
for each assignment.

In order to set up a team, you will have to communicate the team
members to a member of the course staff so they can create the team
for you.

Once your team is established, you can create a repository for your
work.  All homework should be done in @bold{private} repositories, so
make sure to create a private repository.

To create your pair's repository, log in to github.com.  Be switch to
switch your account context to CCIS-2510H.  Create a new repository.
The name of repository should be @tt{pairNN} where @tt{NN} is your
two-digit pair number.  The repository should be marked private.
Since we've already created teams, your partner should immediately
have access to this repository.  At this point, you need to clone the
repository to your computer and can start working.

Within your repository you should follow the following naming
conventions and directory structure:

@indented[@verbatim{
pairNN/
  assign01/
  assign02/
  ...
}]

Feel free to also add other directories for labs, notes, or whatever
else you find useful, but assignments must be in their own
directories, named as above.

@section*{Important git concepts}

There are a few important git concepts you need to be comfortable
with.  To begin with, there are ``repositories''.  At first cut, a
repository is just a bundle of files that have a history of
modifcations.  When you created your pair repository, you made an
empty bundle with no history.  That repository lives on github.  In
order to make changes, you will need to bring that repository to your
local computer, make changes, commit them, and then communicate those
commits back to github.com.

@itemize[

@item{Clone - cloning is a one-time operation you do to copy a
repository from a server to your computer.  Once you've done this, you
now have the repository on your local machine.  You can add, edit, or
delete files within this repository as you would any other set of
files.

Once you've made changes that you want to save to the repository,
you're ready for a commit.}

@item{Commit - when you're ready to save your changes to your local
repository, you need to commit them.  You can select any files in the
repository that have been modified for a commit.  You can also add new
files to the repository or remove files.  Once you've selected what
you want to save, you can commit giving an informative message about the change.

At this point, it's important to remember that you've made changes
@emph{only} to your @bold{local} repository.  Your team members cannot
see these changes.  If your hard drive failed or your laptop got
stolen, those changes would be gone forever.

So once you've committed (perhaps several times) and are ready to save
this batch of commits to the server---and make them available to other
team members---you need to push.}

@item{Push - a push communicates a batch of commits on your computer
to a server.  Once commits have been pushed, they are forever a part
of the history of the repository.  Team members (and graders) can see
the changes, and if something happens to your computer, at least those
commits you've pushed will still be available on the server.}

@item{Pull - a pull communicates a batch of commits on the server to
your computer.  A pull is how you update your local repository will
changes your team members have made.  It's a good idea to always do a
pull before you start any new work to avoid potential conflicts.}
]

There are a couple things that you want to make sure you do.

@itemize[

@item{Commit often.  Push often.  There is nothing worse than losing
work and this will help minimize the awfulness in case something bad
happens to your computer.}

@item{Don't forget to explicitly add new files to your repository.  If
you create a new file within your repository, this is not actually a
part of the repository until you add and commit it.}

@item{Write informative commit messages.  Documenting the evolution of
your work will help you understand what you were thinking when months
later you're trying to make a change to your code.  It will also help
other people (partners, graders, users) understand your project.  Good
programmers always write informative commit messages and value others
who do the same.  Messages such as ``some changes'' or ``fixed some
stuff'' convey nothing useful.}
]




