#lang scribble/manual

@(require "../lab.rkt"
          "../unnumbered.rkt"
          "../utils.rkt")

@(define exercise (exercise-counter))

@title[#:tag "lab01"]{5/8: A Little Eclipse, A Little Java}

The goal of this lab is to introduce some aspects of our work
environment: the Eclipse IDE and the basics of running a program in
Java-like languages. In the second part of the lab, (the one that
really teaches you something) will focus on data definitions and
examples in a simple subset of Java language.


@lab:section{Partner selection}


@exercise{
  Find your @seclink["partners1"]{homework partner}
}


@lab:section{Eclipse IDE and a simple Java-like language}

Eclipse includes an editor and allows you to organize your work into
many files that together make up a project. It has an "incremental"
compiler that so you can edit and run your programs while getting
relatively fast error feedback. Your Eclipse workspace can contain
many projects, so you should be able to keep all your work in one
workspace, with one project for each assignment or lab.

@lab:section{Setting up}

