#lang scribble/manual
@(require scribble/core
	  scriblib/footnote
          scribble/decode
          scribble/html-properties
	  "defns.rkt"
          "utils.rkt")

@(define (blockquote . strs)
   (make-nested-flow (make-style "blockquote" '(command))
                     (decode-flow strs)))


@(define accessible
   (style #f (list (js-addition "accessibility.js")
                   (attributes '((lang . "en"))))))

@title[#:style accessible @courseno]{: Make Your Own Video Games: An Introduction to Programming and Computing}

@image[#:scale 1/2 #:style float-right]{img/pacman-chomp.gif}

@emph{Summer, July 8-26 @year, A
@link["https://oes.umd.edu/middle-high-school-students/terp-young-scholars"]{Terp
Young Scholars} course.}

@emph{Lectures: Weekdays 10:00am--1:00pm, 2207 @IRB}

@emph{Professor: @link["https://www.cs.umd.edu/~dvanhorn/"]{David Van Horn}}


@courseno is an introduction to computing and programming. Its major
goal is to introduce students to the principles of systematic problem
solving through programming and the basic rules of computation. It
helps students engage in systematic thought, planning, and universally
useful problem-solving skills applied in the setting of programming
and computation.

@;{
@tabular[#:style 'boxed
         #:sep @hspace[1]
	 #:row-properties '(top)
	 (list (list @bold{Location} @elem{2207 @IRB})
               (list @bold{Time} @elem{Weekdays, 10:00am--1:00pm}))]
}

@tabular[#:style 'boxed 
         #:row-properties '(bottom-border ())
	 (list (list @bold{Staff} 'cont 'cont 'cont)
	       (list @bold{Name} @bold{Office} @elem{@bold{E-mail}} @elem{@bold{Hours}})
	       (list @link["https://www.cs.umd.edu/~dvanhorn"]{David Van Horn} @elem{5250 @IRB} "dvanhorn@cs.umd.edu" "By appt.")
         (list "Samuel Howard" @elem{2207 @IRB} "showard7@umd.edu" "2-3PM Weekdays")
         (list "Angela Lamb" @elem{2207 @IRB} "alamb123@umd.edu" "9-10AM Weekdays"))]

After taking this course, students will have a sense of the
complexities involved in developing software and ought to be able to
use the principles of programming to solve many non-computational
problems in a systematic manner.

This course exposes students to the fundamental techniques of program
design: ``an approach to the creation of software that relies on
systematic thought, planning, and understanding from the very
beginning, at every stage and for every step''
(@hyperlink["https://htdp.org/2018-01-06/Book/part_preface.html"]{HtDP/2e,
Preface}).  While taking this course will @emph{not} make you a great
programmer, you cannot become a great programmer without mastering
these skills.  More importantly, even if you never program again, a
student of design ``will still pick up universally useful
problem-solving skills, experience a deeply creative activity, and
learn to appreciate a new form of aesthetic.''

@bold{Assumptions:} The course does @emph{not} assume @emph{any} prior
programming experience. It is therefore suitable for all students who
wish to explore the intellectual ideas of computer science. It does
assume familiarity with (high school) arithmetic and algebra, and it
demands curiosity, self-discipline, and willingness to work with
others.

@bold{Disclaimer:} All information on this web page is tentative and
subject to change until the start of the semester.

@(define IRB (link "https://maps.google.com/maps?q=BRENDAN+IRIBE+CENTER%2c+College+Park%2c+MD&z=18" "IRB"))

@include-section{syllabus.scrbl}
@include-section{schedule.scrbl}
@include-section{texts.scrbl}
@include-section{assignments.scrbl}
@include-section{exercises.scrbl}
@include-section{labs.scrbl}
@include-section{exams.scrbl}
@include-section{catharsis.scrbl}

@;{

@;include-section{notes.scrbl}
@;include-section{style.scrbl}

@;{section[#:style 'unnumbered]{Piazza}

All announcements will be made on @link["http://piazza.com/umd/fall2018/cmsc131a"]{Piazza}.  Please sign-up at the start of the semester.

Emergency announcement such as last-minute class cancelations (which
should not happen often), will also be announced via the university
email system.

@section[#:style 'unnumbered]{Grades Server}

All grades will be posted on the Grades server.

@url{http://grades.cs.umd.edu/}

@section[#:style 'unnumbered]{Submit Server}

@url[(string-append "http://submit.cs.umd.edu/" semester year)]

@;include-section{resources.scrbl}
@include-section{acks.scrbl}

}
}