#lang scribble/manual
@(require scribble/core 
          scribble/examples
          racket/sandbox
          (for-label lang/htdp-beginner) 
          (for-label (except-in 2htdp/image image?) (except-in 2htdp/universe name))
          ;"helper.rkt" 
	  "../utils.rkt"
          "../defns.rkt")

@(define-syntax-rule (result e) 
   @examples[#:eval the-eval #:result-only e])


@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require (only-in lang/htdp-intermediate check-expect empty? define-struct cons? first rest cons explode)))
    (the-eval '(require 2htdp/image))
    (the-eval '(require (only-in lang/htdp-beginner identity string-whitespace?)))
    (the-eval '(require (prefix-in r: racket)))
the-eval))


@examples[#:eval the-eval #:hidden
;; place code here
]

@title[#:style 'unnumbered #:tag "lab8"]{Lab 8: Files and Directories}

@(define ex (make-exerciser "Lab problem"))


@section[#:tag "lab8intro"]{Introduction(s)}

You'll work in labs in pairs.  Find someone to work with for this
first lab and introduce yourself. 

Make sure at least one of you have a laptop to work on for this lab.

The two of you will work as a team to solve problems. At any time, one of you
will be the @bold{Head} and the other will be the @bold{Hands}. The @bold{Head}
does the thinking and the @bold{Hands} does the typing. @bold{Hands} type only
what the @bold{Head} tells them to, but you're free to discuss any issues that
pop up. We'll have you switch off during the lab to make sure each of you get
practice problem solving, dealing with syntax, and getting finger exercises on
the keyboard.

@section[#:tag "lab8:purpose"]{Purpose}

In this lab, you'll practice designing programs for self-referential
data (that are not lists).

@section{The Structure of File Systems}

Almost every computing device that comes with persistent data storage
allowing you to save your stuff, organizes that data into notions of
``files'' and ``directories.''  You're probably fairly comfortable
with this organization and have no problem navigating from, say, your
home directory down many layers of nested directories within
directories within directories to find some file you need.

Today we're going to look at how to represent and operate over this
kind of data.

Think for a moment about what you know about files and directories
(aka folders).

@itemlist[

@item{Files and directories are distinct things.}

@item{Files and directories have names.}

@item{A file has data inside it (the contents of the file).}

@item{A directory has files and directories inside it.}

@item{A directory may contain an arbitrary number of files or directories inside it.}

@item{Directories may nest arbitrarily deeply.}

]

This suggests the following data definition for representing files and directories.

@#reader scribble/comment-reader (racketblock
(define-struct file (name content))
;; A File is a (make-file String String).
;; Interp: Represents a file with a name and some arbitrary content.
;;
(define-struct dir (name contents))
;; A Directory is a (make-dir String LoFileOrDir))
;; Interp: Represents a named container for an arbitrary amount of files or
;;         directories.
;;

;; A LoFileOrDir is one of:
;; - '()
;; - (cons FileOrDir LoFileOrDir)

;; A FileOrDir is one of:
;; - File
;; - Directory
;; Interp: Either a file or directory.
)

@ex["Templates"]{

Write down the template for each of @emph{File},
@emph{Directory}, @emph{FileOrDir}, and @emph{[LoFileOrDir}. Be sure to
reference the @tt{file-template} and the @tt{directory-template} in your
@tt{file-or-dir-template}.

}

Let's make some examples.  Suppose we have the
following filesystem structure:
@verbatim|{
/
 DIR0/
      FILE0
 DIR1/
      FILE1
      DIR2/
           FILE2
           FILE3
           FILE4
 DIR3/
}|
where FILE0 has the following content:
@verbatim|{To be, or not to be: that is the question}|
FILE1 has the following content:
@verbatim|{This above all: to thine own self be true}|
FILE2 has the following content:
@verbatim|{It is a tale told by an idiot, full of sound and fury, signifying nothing.}|
FILE3 has the following content:
@verbatim|{Some are born great, some achieve greatness, and some have greatness thrust upon 'em.}|
and FILE4 has no content at all.

To get you started, here are the examples representing the root directory
@tt{ROOTDIR}, @tt{DIR3}, @tt{DIR0}, @tt{FILE4}, and @tt{FILE0}:


@ex["Examples"]{

Complete the definitions to represent the example given above.

@#reader scribble/comment-reader (racketblock
(define FILE4 (make-file "FILE4" ""))
;; (define FILE3 ...)
;; (define FILE2 ...)
;; (define FILE1 ...)
(define FILE0
  (make-file "FILE0" "To be, or not to be: that is the question"))

(define DIR3 (make-dir "DIR3" '()))
;; (define DIR2 ...)
;; (define DIR1 ...)
(define DIR0 (make-dir "DIR0" (list FILE0)))
(define ROOTDIR (make-dir "" (list DIR0 DIR1 DIR3)))
)

}

@section[#:style 'unnumbered #:tag "lab8:count"]{Counting Files and Directories}

@ex[@racket[num-children]]{ 

Design a function @tt{num-children} that returns the
number of files or directories found directly inside the given directory.

@#reader scribble/comment-reader (racketblock
(check-expect (num-children ROOTDIR) 3)
(check-expect (num-children DIR0) 1)
(check-expect (num-children DIR3) 0)
)

}

@ex[@racket[num-descendents]]{

Design a function @tt{num-descendents} that returns the
number of files or directories found at any level inside the given directory.

@#reader scribble/comment-reader (racketblock
(check-expect (num-descendents ROOTDIR) 9)
(check-expect (num-descendents DIR0) 1)
(check-expect (num-descendents DIR1) 5)
)

}

@section[#:style 'unnumbered #:tag "lab8:find"]{Looking for Files and Directories}

@;{Swap @bold{Head} and @bold{Hands}!}

@ex[@racket[file-exists?]]{

Design a function @tt{file-exists?} that, given a
directory and a string @tt{name}, returns @racket[#true] if a file
with the given @tt{name} exists inside the given directory or any of its
subdirectories.

}

@ex[@racket[dir-exists?]]{

Design a function @tt{dir-exists?} that, given a
directory and a string @tt{name}, returns @racket[#true] if a directory
with the given @tt{name} exists inside the given directory or any of its
subdirectories.

}

@ex[@racket[file-or-dir-exists?]]{

Design a function @tt{file-or-dir-exists?} that, given a
directory and a string @tt{name}, returns @racket[#true] if a file or directory
with the given @tt{name} exists inside the given directory or any of its
subdirectories.

}

@section[#:style 'unnumbered #:tag "lab8:list"]{Listing Files and Directories}

@ex[@racket[all-file-names]]{

Design a function @tt{all-file-names} that returns a list
of file names found inside the given directory and any of its
subdirectories. The order in which the file names are returned is not
important.

}

@ex[@racket[all-dir-names]]{

Design a function @tt{all-dir-names} that returns a list
of directory names found inside the given directory and any of its
subdirectories. The order in which the directory names are returned is not
important.

}

@ex[@racket[all-names]]{

Design a function @tt{all-names} that returns a list of
all file or directory names found inside the given directory and any of its
subdirectories. The order in which the file or directory names are returned is
not important.

}

@section[#:style 'unnumbered #:tag "lab8:look"]{Looking at Files in Directories}

@;{Swap @bold{Head} and @bold{Hands}!}

@ex[@racket[file-size]]{

Design a function @tt{file-size} that returns the number
of characters inside the content of the given file.

@#reader scribble/comment-reader (racketblock
(check-expect (file-size FILE4) 0)
(check-expect (file-size FILE0) 41)
(check-expect (file-size (make-file "foo" "foo!")) 4)
)

}

@ex[@racket[dir-size]]{

Design a function @tt{dir-size} that returns the sum of
the sizes of all files inside the given directory and its subdirectories.

}