#lang scribble/manual
@(require scribble/core
	  scriblib/footnote
          scribble/decode)

@(define (blockquote . strs)
   (make-nested-flow (make-style "blockquote" '(command))
                     (decode-flow strs)))

@(define year "2017")
@(define courseno "CMSC 198")

@title[@courseno]{: Systematic Program Design I}

@emph{Fall, @year}

@margin-note*{@emph{Program design} is the study of systematic
thought, planning, and universally useful problem-solving skills
applied in the setting of programming and computation.}

Masterful programmers design programs the way Jacques Pépin makes an
omellete: with systematic technique, honed creativity, and a strong
aesthetic (``There's the Wrong Way and Jacques Pépin’s Way,''
@hyperlink["http://www.nytimes.com/2011/10/19/dining/jacques-pepin-demonstrates-cooking-techniques.html"]{New
York Times, Oct. 18, 2011}).

This course exposes students to the fundamental techniques of program
design: ``an approach to the creation of software that relies on
systematic thought, planning, and understanding from the very
beginning, at every stage and for every step''
(@hyperlink["http://www.ccs.neu.edu/home/matthias/HtDP2e/part_preface.html"]{HtDP/2e,
Preface}).  While taking this course will not make you a great
programmer, you cannot become a great programmer without mastering
these skills.  More importantly, even if you never program again, a
student of design ``will still pick up universally useful
problem-solving skills, experience a deeply creative activity, and
learn to appreciate a new form of aesthetic.''

The goal of @courseno is to arm students with the ability to design,
implement, and extend a programming language. Throughout the course,
students will design and implement several related languages, and will
explore parsing, syntax querying, dataflow analysis, compilation to
bytecode, type systems, and language interoperation.

@(define (bldg abbr)
   (link (string-append "http://www.umd.edu/CampusMaps/bld_detail.cfm?bld_code=" abbr) abbr))
@(define AVW "AVW")
@(define CSI "CSI")

@tabular[#:style 'boxed 
         #:row-properties '(bottom-border ())
	 (list (list @bold{Staff} 'cont 'cont 'cont)
	       (list @bold{Name} @bold{Office} @elem{@bold{E-mail} (@"@cs.umd.edu")} @bold{Office Hours})
	       (list @link["https://www.cs.umd.edu/~dvanhorn"]{David Van Horn} @elem{3439 @AVW} "dvanhorn" @elem{TBD})
         (list "Nicholas Labich" @elem{4103 @AVW} "labichn" @elem{TBD}))]

@tabular[#:style 'boxed
         #:sep @hspace[1]
	 #:row-properties '(top)
		 (list (list @bold{Location} @elem{TBD})
	       (list @bold{Time} @elem{TBD})
	       (list @bold{Midterm 1} "TBD, in class")
	       (list @bold{Midterm 2} "TBD, in class")
	       ;(list @nonbreaking{@bold{Final Exam}} @elem{@link["http://www.registrar.umd.edu/current/registration/exam%20tables%20spring.html"]{Monday, May 16, 10:30-12:30pm}, 1122 CSI})
	       (list @bold{Textbooks} @elem{@emph{How to Design Programs}, Felleisen, et al.}))]

@;include-section{schedule.scrbl}

@section[#:style 'unnumbered]{Announcements}

All announcements will be made on @link["http://piazza.com/umd/spring2017/cmsc430"]{Piazza}.  Please sign-up at the start of the semester.

Emergency announcement such as last-minute class cancelations (which
should not happen often), will also be announced via the university
email system.

@section[#:style 'unnumbered]{Grades}

@url{http://grades.cs.umd.edu/}

@section[#:style 'unnumbered]{Submit}

@url[(string-append "http://submit.cs.umd.edu/spring" year)]

@;include-section{syllabus.scrbl}
@;include-section{projects.scrbl}
@;include-section{homeworks.scrbl}
@;include-section{resources.scrbl}

@;include-section{texts.scrbl}
@;include-section{schedule.scrbl}
@;include-section{research-project.scrbl}
@;include-section{problem-sets.scrbl}
@;include-section{blog.scrbl}

