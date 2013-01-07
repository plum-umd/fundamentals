#lang scribble/manual
@(require racket/list)
@(require "unnumbered.rkt" "utils.rkt")

@(define instructors
  (list (list @link[#:style "plainlink" "http://www.ccs.neu.edu/home/samth/"]{Sam Tobin-Hochstadt}
	      (tt "samth") "WVH358 " "Thursday " "3:30-5:30pm")
        (list @link[#:style "plainlink" "http://www.ccs.neu.edu/home/dvanhorn/"]{David Van Horn} 
	      (tt "dvanhorn") "WVH350" "Monday " "3:30-5:30pm")))

@(define tas
  (list (list "Nick Labich"
	      (tt "labichn") "WVH102" "Monday " "7:45-9:45pm")))

@(define tutors
  (list (list "Kathleen Mullins" (tt "mullinsk") "WVH102" "Saturday " "noon-2:00pm")
        (list "Becca MacKenzie" (tt "rmacnz") "WVH102" "Tuesday " "6:30pm-8:30pm")
        #;#;
        (list "Sarah Laplante" (tt "laplante") "WVH102" "Wednesday " "11:30am-1:30pm")
        (list "Ryan Plessner" (tt "rpless") "WVH102" "Saturday " "3:00-5:00pm")))

@; commas : [List X] -> [List (U X String)]
@(define (commas xs)
   (cond [(= 2 (length xs)) (list (first xs) " and " (second xs))]
         [else (add-between xs ", ")]))

@title*{General}

@section*{People}

@indented{
@bold{Instructors:} @(apply elem (commas (map first instructors))).

Instructors design and implement this class.  They lecture and create
the labs, assignments, and exams.  They are here to
@link["http://worrydream.com/SomeThoughtsOnTeaching/"]{teach from
life}.

@bold{TAs:} @(apply elem (commas (map first tas))).

TAs teach labs, supervise the grading of homework sets, hold office
hours, and occasionally substitute in lectures. In general, they are
apprentice teachers and are here to learn how to run a course.

@bold{Tutors:} @(apply elem (commas (map first tutors))).

Tutors have, in the past year or two, mastered the material of this
course; they help in labs, hold office hours, grade assignments, and 
can remember what it's like to be lost.  They are here to gain a
deeper understanding by teaching what they know to others.

}

@section*{Office Hours}

@indented[@tabular[(append instructors tas tutors)]]

@bold{Communication}

@indented{Use CCIS email (@tt["@ccs.neu.edu"]) to reach
any of the course staff; usernames are given above.}

@bold{Lectures} 
@indented{Monday and Thursday 11:45–1:25pm in WVH 108.}

@bold{Labs}  
@indented{Monday 6:00–7:40pm in WVH 210.}


@section*{Policies}

@itemlist[
  @item{Late policy: there is no late policy; solutions to 
        assignments as they exist at the time of the deadline will be graded.}
  @item{Laptop policy: no laptops in class.}
  @item{Academic honesty: we will strictly enforce Northeastern's
  @link[#:style "plainlink"
  "http://www.northeastern.edu/osccr/academichonesty.html"]{academic
integrity policy}.  All programs must be completed strictly by you and
your partner.  You are free to discuss the problem sets with others,
so long as you acknowledge discussants.  However, @emph{you may not
share code in any way}.  Submitting code that is not your own will be
considered a violation of the University's Academic Integrity
Policy (pages 38---40 of the @link[#:style "plainlink"
"http://www.northeastern.edu/osccr/pdfs/Code%20of%20Conduct/2011_2012_Code_of_Co.pdf"]{2011-2012
Undergraduate Student Handbook}).  Violations of academic integrity will
be reported to @link[#:style "plainlink"
"http://www.northeastern.edu/osccr/"]{OSCCR} and will have a negative
impact on your grade.}]

@section*{Computing Environment}

@indented{We will use @link[#:style "plainlink"
"http://racket-lang.org/"]{DrRacket} v5.3.1.  DrRacket is installed on
the CCS computers. It is also freely available on the web in case you
wish install it on your own computer.

You will need to install our @seclink["class"]{course software} into
DrRacket. In the DrRacket @tt{File} menu, select @tt{Install PLT File}, and then
enter the url

@centered[@tt[class-system-latest]]


You will use @secref{Git} to work on your homework sets, to
keep track of revisions, and to submit your homework.}

@section*{Bug Reports}

@indented{This course relies on several peices of software including the
@seclink["class"]{class system}, the @seclink["book"]{book}, and this
web page.  You can follow the development of this software on Github:

@centered{@tt{@url{https://github.com/dvanhorn/dpc}}}

and you should submit bugs by creating a new
"@link["https://github.com/dvanhorn/dpc/issues"]{issue}".  Your help
in improving the software and book are greatly needed and appreciated.}

@section*{Problem Sets}

@indented{There will be weekly problem sets. 

We will drop the homework grade with the worst impact on your final
grade from consideration for the final grade. Thus, if you just don't
get it one week, nothing is lost. The story is different for the
second or third or ... time.}

@section*{Pair Programming}

@indented{You must work on your problem sets in pairs. Your lab TA will assign
you a partner. Every few weeks, you will get a new partner.

Pair programming means that you and your partner work on the problem
sets jointly. You read them together and you work on the solutions
together. One of the lab's purposes is to teach you how to work in
pairs effectively; indeed, pairs are provably more effective than
individuals in programming. The rough idea is this: One of you plays
@emph{pilot}, the other @emph{co-pilot}. The pilot works on the
keyboard and explains aloud what is going on; it is the co-pilot's
responsibility to question everything. After a problem is solved to
the satisfaction of both, you @emph{must} switch roles.
}

@section*{Exams}

@itemlist[
  @item{Exam 1: TBD at 6-9 PM}
  @item{Exam 2: TBD at 6-9 PM}]

@section*{Projects}

@indented{There will be a substantial class project implemented over the last
several weeks of the course.}

@section*{Grades}

@indented{You will get a gpa for your homework (including the project) and for
your exams. You must have both a passing homework gpa and a passing
gpa to pass the course. For the final grade, we will assign a weight
of 40% to the homework grade and a weight of 55% to the two exams. The
remaining 5% are up to the instructors' whim.}
