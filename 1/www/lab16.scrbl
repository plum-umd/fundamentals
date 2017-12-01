#lang scribble/manual
@(require scribble/core (for-label lang/htdp-intermediate) "helper.rkt")

@title[#:style 'unnumbered #:tag "lab16"]{Lab 16: Lists qua Filesystem}

Implement this lab with the
@link["https://docs.racket-lang.org/htdp-langs/intermediate.html"]{Intermediate Student
Language}.

Make sure you follow
@link["https://cs.umd.edu/class/fall2017/cmsc131A/style.html"]{The Style} we use
for the {B,I,A}SL{,+} languages in this class.

Choose the initial @bold{Head} and @bold{Hands}, and get started!


@section[#:style 'unnumbered #:tag "lab16:dd"]{A List by Any Other Name}

Directories are a bit like lists with some metadata.

@#reader scribble/comment-reader (racketblock
(define-struct file (name content))
;; A File is a (make-file String String).
;; Interp: Represents a file with a name and some arbitrary content.
;;
(define-struct dir (name contents))
;; A Directory is a (make-dir String [Listof FileOrDir])
;; Interp: Represents a named container for an arbitrary amount of files or
;;         directories.
;;
;; A FileOrDir is one of:
;; - File
;; - Directory
;; Interp: Either a file or directory.
)

@larger{@bold{Ex 1}}: Write down the template for each of @emph{File},
@emph{Directory}, @emph{FileOrDir}, and @emph{[Listof FileOrDir]}. Be sure to
reference the @tt{file-template} and the @tt{directory-template} in your
@tt{file-or-dir-template}.

@larger{@bold{Ex 2}}: Define example files and directories to represent the
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

@#reader scribble/comment-reader (racketblock
(define FILE4 (make-file "FILE4" ""))
;; (define FILE3 ...)
;; (define FILE2 ...)
;; (define FILE1 ...)
(define FILE0 (make-file "FILE0" "To be, or not to be: that is the question"))

(define DIR3 (make-dir "DIR3" '()))
;; (define DIR2 ...)
;; (define DIR1 ...)
(define DIR0 (make-dir "DIR0" (list FILE0)))
(define ROOTDIR (make-dir "" (list DIR0 DIR1 DIR3)))
)


@section[#:style 'unnumbered #:tag "lab16:count"]{Counting Files and Directories}

@larger{@bold{Ex 3}}: Design a function @tt{num-children} that returns the
number of files or directories found directly inside the given directory.

@#reader scribble/comment-reader (racketblock
(check-expect (num-children ROOTDIR) 3)
(check-expect (num-children DIR0) 1)
(check-expect (num-children DIR3) 0)
)


@larger{@bold{Ex 4}}: Design a function @tt{num-descendents} that returns the
number of files or directories found at any level inside the given directory.

@#reader scribble/comment-reader (racketblock
(check-expect (num-descendents ROOTDIR) 9)
(check-expect (num-descendents DIR0) 1)
(check-expect (num-descendents DIR1) 5)
)


@section[#:style 'unnumbered #:tag "lab16:find"]{Looking for Files and Directories}

Swap @bold{Head} and @bold{Hands}!

@larger{@bold{Ex 5}}: Design a function @tt{file-exists?} that, given a
directory and a string @tt{name}, returns @racket[#true] if a file
with the given @tt{name} exists inside the given directory or any of its
subdirectories.

@larger{@bold{Ex 6}}: Design a function @tt{dir-exists?} that, given a
directory and a string @tt{name}, returns @racket[#true] if a directory
with the given @tt{name} exists inside the given directory or any of its
subdirectories.

@larger{@bold{Ex 7}}: Design a function @tt{file-or-dir-exists?} that, given a
directory and a string @tt{name}, returns @racket[#true] if a file or directory
with the given @tt{name} exists inside the given directory or any of its
subdirectories.


@section[#:style 'unnumbered #:tag "lab16:list"]{Listing Files and Directories}

@larger{@bold{Ex 8}}: Design a function @tt{all-file-names} that returns a list
of file names found inside the given directory and any of its
subdirectories. The order in which the file names are returned is not
important.

@larger{@bold{Ex 9}}: Design a function @tt{all-dir-names} that returns a list
of directory names found inside the given directory and any of its
subdirectories. The order in which the directory names are returned is not
important.

@larger{@bold{Ex 10}}: Design a function @tt{all-names} that returns a list of
all file or directory names found inside the given directory and any of its
subdirectories. The order in which the file or directory names are returned is
not important.


@section[#:style 'unnumbered #:tag "lab16:look"]{Looking at Files in Directories}

Swap @bold{Head} and @bold{Hands}!

@larger{@bold{Ex 11}}: Design a function @tt{file-size} that returns the number
of characters inside the content of the given file.

@#reader scribble/comment-reader (racketblock
(check-expect (file-size FILE4) 0)
(check-expect (file-size FILE0) 41)
(check-expect (file-size (make-file "foo" "foo!")) 4)
)

@larger{@bold{Ex 12}}: Design a function @tt{dir-size} that returns the sum of
the sizes of all files inside the given directory and its subdirectories.


@section[#:style 'unnumbered #:tag "lab16:abs"]{Bonus: Abstract Operations on
Files in Directories}

These are a bit trickier, but if you follow your templates you shouldn't have
too much of a problem.

@larger{@bold{Ex 13}}: Design a function @tt{map-files : [File -> X] Directory
-> [Listof X]} that, given a function @tt{file->x} and a directory, returns a
list containing the results of applying @tt{file->x} to each file in the given
directory or any of its subdirectories.

@larger{@bold{Ex 14}}: Design a function @tt{filter-files : [File -> Boolean]
Directory -> [Listof File]} that, given a function @tt{test?} and a directory,
returns a list all files in the given directory and any of its subdirectories
for which @tt{test?} applied to that file is @racket[#true].

@larger{@bold{Ex 15}}: Design a function @tt{fold-files : [File X -> X] X
Directory -> [Listof X]} that, given a function @tt{combine : File X -> X}, a
base value @tt{X} and a directory, returns the result of applying @tt{combine} to
each file and recursive result of @tt{fold-files} in the given directory and any
of its subdirectories.

@larger{@bold{Ex 16}}: Reimplement the functions @tt{map-files} and
@tt{filter-files} in terms of @tt{fold-files}.

@colorize["red"]{@bold{Hint}}: If you've never implemented the list operations
@racket[map] and @racket[filter] in terms of @racket[foldr], do that first!
