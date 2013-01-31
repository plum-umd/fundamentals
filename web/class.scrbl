#lang scribble/manual
@(require "unnumbered.rkt"
          "utils.rkt")

@title*[#:tag "class" #:style '(toc)]{Class system}

@local-table-of-contents[]

@include-section[(lib "class/universe.scrbl")]
@include-section[(lib "class/0/0.scrbl")]
@include-section[(lib "class/1/1.scrbl")]
@internal[
@include-section[(lib "class/2/2.scrbl")]
@include-section[(lib "class/3/3.scrbl")]
@include-section[(lib "class/4/4.scrbl")]
@include-section[(lib "class/5/5.scrbl")]
]
The latest class system is:

@indented{@class-system-filename}

It can be accessed at either of the following URLs:

@indented{
  @link[class-system-latest]{@class-system-latest}

  @link[class-system-url]{@class-system-url}}
