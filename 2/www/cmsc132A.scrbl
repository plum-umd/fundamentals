#lang scribble/manual
@(require scribble/core
	  scriblib/footnote
          scribble/decode
	  "defns.rkt")

@(define (blockquote . strs)
   (make-nested-flow (make-style "blockquote" '(command))
                     (decode-flow strs)))

@title[@courseno]{: Systematic Program Design II}

@emph{Spring, @year}

@margin-note*{@emph{Program design} is the study of systematic
thought, planning, and universally useful problem-solving skills
applied in the setting of programming and computation.}

@courseno studies the class-based program design and the design of
abstractions that support the design of reusable software and
libraries. It covers the principles of object oriented program design,
the basic rules of program evaluation, and examines the relationship
between algorithms and data structures, as well as basic techniques
for analyzing algorithm complexity.

The course is suitable for both CS majors and non-majors. It assumes
that student has been introduced to the basic principles of program
design and computation (e.g. Systematic Program Design I).

@margin-note*{@bold{Caveat:} Note that @courseno is being offered on a
trial basis as an alternative introductory course sequence.  You
@emph{must} have taken CMSC 131A previously.  Students who complete
the 131A-132A sequence will be fully prepared for all subsequent
courses that list 131-132 as prerequisites.}

The goal is to help you understand the principles of class-based
program design using an object-oriented programming language(s), not
just Java. Java is used so we can learn how the principles are used in
practical applications, and gives us an opportunity to discuss the
strengths and weaknesses of languages and paradigms.


@bold{Assumptions:} The course assumes proficiency with the systematic
design of programs and some mathematical maturity. It demands
curiosity and self-driven exploration and requires a serious
commitment to practical hands-on programming.

@bold{Disclaimer:} All information on this web page is tentative and
subject to change until the start of the semester.

@(define (bldg abbr)
   (link (string-append "http://www.umd.edu/CampusMaps/bld_detail.cfm?bld_code=" abbr) abbr))
@(define AVW "AVW")
@(define CSI "CSI")

@tabular[#:style 'boxed
         #:sep @hspace[1]
	 #:row-properties '(top)
		 (list (list @bold{Location} @elem{2207 IRB})
	       (list @bold{Time} @elem{MWF 11:00pm--11:50pm})
	       (list @bold{Midterm 1} @elem{@m1-date, in class})
	       (list @bold{Midterm 2} @elem{@m2-date, in class})
               (list @bold{Final exam} @elem{@final-date})
	       ;(list @bold{Textbooks} @elem{@link["http://www.ccs.neu.edu/home/matthias/HtDP2e/"]{@emph{How to Design Programs}, 2nd edition}, Felleisen, et al.})
)]

@tabular[#:style 'boxed 
         #:row-properties '(bottom-border ())
	 (list (list @bold{Staff} 'cont 'cont)
	       (list @bold{Name} @bold{Office} @elem{@bold{E-mail}})
	       (list @link["https://www.cs.umd.edu/~dvanhorn"]{David Van Horn} @elem{3439 @AVW} "dvanhorn@cs.umd.edu")
	 (list "Deena Postol" @elem{1112 @AVW} " dpostol@umd.edu")
	 (list "William Daseking" @elem{1112 @AVW} "wdasekin@terpmail.umd.edu"))]


@tabular[#:style 'boxed
         #:row-properties '(bottom-border ())
         (list (list @bold{Office hours} 'cont 'cont)
               (list @bold{Monday} 'cont 'cont)
	       (list "3-4pm" "1112 AVW" "William")
               (list "4-5pm" "2207 IRB" "In Lab")
	       (list @bold{Tuesday} 'cont 'cont)
               (list "2-3pm" "1112 AVW" "Deena")
	       (list @bold{Wednesday} 'cont 'cont)
               (list "2-3pm" "1112 AVW" "Deena")
	       (list "4-5pm" "2207 IRB" "In Lab")
	       (list @bold{Thursday} 'cont 'cont)
	       (list "4-5pm" "1112 AVW" "William")
	       (list @bold{Friday} 'cont 'cont)
	       (list "5250 IRB" "3-4pm" "David"))]

@include-section{syllabus.scrbl}
@include-section{texts.scrbl}
@include-section{schedule.scrbl}
@include-section{exams.scrbl}
@include-section{lectures.scrbl}
@include-section{labs.scrbl}
@include-section{assignments.scrbl}
@;include-section{notes.scrbl}
@;include-section{style.scrbl}

@section[#:style 'unnumbered]{Piazza}

All announcements will be made on @link[piazza-url]{Piazza}.  Please sign-up at the start of the semester.

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


