#lang scribble/manual
@(require racket/list)
@(require "../unnumbered.rkt" "../utils.rkt")

@(define instructors
  (list (list @link[#:style "plainlink" "http://www.ccs.neu.edu/home/dvanhorn/"]{David Van Horn}
	      (tt "dvanhorn") "WVH350" "Thursday " "3:30pm-5:30pm")))

@(define tas
  (list (list @link[#:style "plainlink" "http://www.ccs.neu.edu/home/ianj/"]{Ian Johnson} (tt "ianj") "WVH308" "Friday " "1:00pm-3:00pm")))

@(define tutors
  (list	(list "Jason Shrand" (tt "jshrand") "WVH102" "Monday " "2:00pm-4:00pm")
        (list "James Grammatikos" (tt "jgramm") "WVH102" "Tuesday" "11:30am-1:30pm")))

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

@itemlist[
@item{Use CCIS email (@tt["@ccs.neu.edu"]) to reach
any of the course staff; usernames are given above.}
@item{Read the course blog on a daily basis.}]

@bold{Lectures} are held Monday, Tuesday, and Wednesday at
9:50am-11:30am in Shillman Hall 215.

@bold{Exams} are held Thursday at
9:50am-11:30am in Shillman Hall 215.

@bold{Labs} are held Tuesday and Thursday at
1:30-3:10pm in 211 WVH.

@bold{Assignments} are due on Monday at
midnight in svn.

@bold{Final Project} is due on Tuesday, June 26 at
midnight in svn.

@section*{Policies}

@itemlist[
  @item{Late policy: there is no late policy; solutions to 
        assignments as they exist at the time of the deadline will be graded.}
  @item{Laptop policy: no laptops in class.}
  @item{Academic honesty: we will strictly enforce Northeastern's
  @link[#:style "plainlink"
  "http://www.northeastern.edu/osccr/academichonesty.html"]{academic
  integrity policy}.  You may discuss problems with other students,
  but you should not share or show code to anyone other than your
  assigned partner.  Violations of academic integrity will be reported
  to OSCCR and will have a negative impact on your grade.}]

@section*{Computing Environment}

@indented{You will complete your assignments (other than the first
one) using the @link[#:style "plainlink"
"http://www.eclipse.org/"]{Eclipse IDE}. Though, if you feel more
comfortable, you may choose to use another IDE (e.g. NetBeans) or work
directly from the command line, but you and your partner must both be
comfortable with the chosen programming environment, and the staff may
not be able to assist you with issues encountered in other
environments.

You will use @secref{Subversion} to work on your homework sets, to
keep track of revisions, and to submit your homework.}

@section*[#:tag "Assignments (general)"]{Assignments}

@indented{There will be one problem set each week.

@; The practice problems will be a series of practice problems that every
@; student must be able to solve. You should work out these problems on
@; your own, and keep your solutions as an electronic portfolio. You may
@; ask the instructor to give an informal review your portfolio at any
@; time, but there will be a formal review at least once during the
@; semester.

Problem sets require you (and a partner) to apply the concepts from
lectures and labs. The problems will consist of structured programming
assignments that may be based on the work done in previous weeks, and
may also include more creative projects where you can practice your
design skills.}

@section*{Pair Programming}

@indented{You must work on your problem sets in pairs. Your lab TA
will assign you a partner. Every few weeks, you will get a new
partner.

Keep a log/calendar documenting the times you and your partner met and
worked together and when you plan to meet next. If the planned meeting
does not happen, write down an explanation.

Pair programming means that you and your partner work on the problem
sets jointly. You read them together and you work on the solutions
together. One of the lab's purposes is to teach you how to work in
pairs effectively; indeed, pairs are provably more effective than
individuals in programming. The rough idea is this: One of you plays
@emph{pilot}, the other @emph{co-pilot}. The pilot works on the
keyboard and explains aloud what is going on; it is the co-pilot's
responsibility to question everything. After a problem is solved to
the satisfaction of both, you @emph{must} switch roles.

Every partner must be able to solve every homework problem in the
end. In other words, You must be able to solve every homework problem
on your own.

All programs must be completed strictly by you and your partner.  You
are free to discuss the problem sets with others, so long as you
acknowledge discussants.  However, @emph{you may not share code in any
way}.  Submitting code that is not your own will be considered a
violation of the University's Academic Integrity Policy (pages 38---40
of the @link[#:style "plainlink"
"http://www.northeastern.edu/osccr/pdfs/Code%20of%20Conduct/2011_2012_Code_of_Co.pdf"]{2011-2012
Underaduate Student Handbook}).  Violations of academic integrity will
be reported to @link[#:style "plainlink"
"http://www.northeastern.edu/osccr/"]{OSCCR} and will have a negative
impact on your grade.


If you are having difficulties working with your partner, please
inform your instructor. Bring in your log of meetings with your
partner to help explain the problems you may have.}

@section*{Exams}

@indented{
There will be a weekly in-class exam that tests your mastery of the
course material.  Exams are open-book and notes.  You may not use any
calculational device other than the one between your ears.}

@section*{Projects}

@indented{There will be a substantial class project implemented over
the last several weeks of the course.}

@section*{Grades}

@indented{You will get a gpa for your homework (including the project)
and for your exams. You must have both a passing homework gpa and a
passing gpa to pass the course. For the final grade, we will assign a
weight of 40% to the homework grade and a weight of 55% to the seven
exams. The remaining 5% are up to the instructors' whim.}
