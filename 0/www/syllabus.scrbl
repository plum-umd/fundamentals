#lang scribble/manual
@(require scribble/core
          "defns.rkt")

@provide[exam-table]

@title[#:style 'unnumbered]{Syllabus}

@local-table-of-contents[]

@section{Prerequisites and Description}

@bold{Corequisite:} None.

@bold{Credits:} 3.

@courseno is an introduction to computing and programming. Its major
goal is to introduce students to the principles of systematic problem
solving through programming and the basic rules of computation.

The course does @emph{not} assume @emph{any} prior
programming experience. It is therefore suitable for all freshman
students---majors and non-majors alike---who wish to explore the
intellectual ideas of the discipline. It does assume familiarity with
(high school) arithmetic and algebra, and it demands curiosity,
self-discipline, and willingness to work with others.

@section{Topics}

The following topics will be covered in this course:


@tabular[#:style 'boxed
	 #:sep @hspace[1] 
	 (list (list @bold{Topics})
	       @list{How to program}
	       @list{How to design}
	       @list{Design with fixed-size data}
	       @list{Design with arbitrarily large data}
	       @list{Design with abstractions}
	       @list{Using abstractions}
	       @list{Nameless functions}
	       @list{Recursion with accumulators})]


@section{Grading}

Grades will be maintained on @link[elms-url]{ELMS}.

You are responsible for all material discussed in lecture and posted
on the class web page, including announcements, deadlines, policies,
etc.

Your final course grade will be determined according to the following
percentages:

@(define grades:m1 (list @elem{Midterm, @m1-date} "10%"))
@(define grades:f  (list @elem{Final Exam, @final-date} "20%"))

@(define (make-grade-component-table . entries)
  @tabular[#:style 'boxed
	   #:sep @hspace[1] 
           (list* (list @bold{Component} @bold{Percentage}) entries)])

@(define exam-table
  @make-grade-component-table[
    @grades:m1
    @grades:f])

@make-grade-component-table[
  (list "Assignments" "30%")
  (list "Exercises" "25%")
  (list @elem{Quizzes & surveys}    "15%")
  @grades:m1
  grades:f]
	 

@section[#:tag "syllabus-assignments"]{Assignments}

There will be @bold{three} weekly programming @secref{Assignments}, due each Friday
at 11:59pm EST.

@section[#:tag "syllabus-exercises"]{Exercises}

There will be @bold{nine} daily or semi-daily @secref{Exercises}, due each Monday,
Tuesday, Wednesday, and Thursday at 11:59pm.

@section[#:tag "syllabus-quiz"]{Quizzes & surveys}

There will be @bold{many} quizzes and surveys.  Some will be done
online, some will be on paper.  Quizzes are graded on a binary scale
(either full or no credit).  Completed surveys receive full credit.
Instructors reserve the right to reject survey responses that are not
considered thoughtful.

@section{Laptops in class}

Class will consist of lecture and lab components.

Laptops will not be permitted in class during lecture.  Laptops will be needed during lab.

@section{Outside-of-class communication with course staff}

Course staff will interact with students outside of class in primarily
three ways: in-person during office hours, electronically via the web
forum and course web page, and electronically via e-mail. The large
majority of communication should employ the first two methods,
reserving e-mail for personal (presumably rare) circumstances.

Personalized assistance, e.g., with assignments or exam preparation,
will be provided during office hours.  Office hours for the
instructional staff will be posted on the course web page.

Additional assistance will provided via the ELMS class discussion
forum. You may use this forum to ask general questions of interest to
the class as a whole, e.g., administrative issues or problem set
clarification questions. The course staff will monitor ELMS on a daily
basis, but do not expect immediate answers to questions. Please do not
post publicly any information that would violate the university
academic integrity policy (e.g., problem set code).

ELMS allows students to send private questions that are only visible
to instructors. Please use this feature if you wish to ask specific
questions concerning your assignment solutions.

Personal e-mail to instructors or TAs should be reserved for issues
that cannot be handled by the above methods.

Important announcements will be made in class or on the class web
page, and via ELMS.

@section{Excused Absences}

Any student who needs to be excused for an absence from a single
lecture or lab due to illness shall:

@itemlist[#:style 'numbered

@item{Make a reasonable attempt to inform the instructor of his/her
illness prior to the class.}

@item{Upon returning to the class, present their instructor with a
self-signed note attesting to the date of their illness. Each note
must contain an acknowledgment by the student that the information
provided is true and correct. Providing false information to
University officials is prohibited under Part 9(h) of the Code of
Student Conduct (V-1.00(B) University of Maryland Code of Student
Conduct) and may result in disciplinary action.}
]

Missing an @bold{exam} for reasons such as illness, religious observance,
participation in required university activities, or family or personal
emergency (such as a serious automobile accident or close relative's
funeral) will be excused so long as the absence is requested in
writing at least @bold{2 days} in advance and the student includes
documentation that shows the absence qualifies as excused; @bold{a
self-signed note} is not sufficient as exams are Major Scheduled
Grading Events. For this class, such events are the final exam, and
the midterm, which will be given in class on the following dates:

@itemlist[
  @item{Midterm: @bold{@m1-date}}
  @item{Final Exam: @bold{@final-date}}]

The final exam is scheduled according to the University Registrar.

For medical absences, you must furnish documentation from the health
care professional who treated you. This documentation must verify
dates of treatment and indicate the timeframe that the student was
unable to meet academic responsibilities. In addition, it must contain
the name and phone number of the medical service provider to be used
if verification is needed. No diagnostic information will ever be
requested. Note that simply being seen by a health care professional
does not constitute an excused absence; it must be clear that you were
unable to perform your academic duties.

It is the University's policy to provide accommodations for students
with religious observances conflicting with exams, but it is the your
responsibility to inform the instructor in advance of intended
religious observances. If you have a conflict with one of the planned
exams, you @bold{must} inform the instructor prior to the end of the
first two weeks of the class.

For missed exams due to excused absences, the instructor will arrange
a makeup exam. If you might miss an exam for any other reason other
than those above, you must contact the instructor @bold{in advance} to
discuss the circumstances. We are not obligated to offer a substitute
assignment or to provide a makeup exam unless the failure to perform
was due to an excused absence.

The policies for excused absences @bold{do not} apply to project
assignments. Projects will be assigned with sufficient time to be
completed by students who have a reasonable understanding of the
necessary material and begin promptly. In cases of @bold{extremely
serious} documented illness of @bold{lengthy duration} or other
protracted, severe emergency situations, the instructor may consider
extensions on project assignments, depending upon the specific
circumstances.

Besides the policies in this syllabus, the University's policies apply
during the semester. Various policies that may be relevant appear in
the @link["http://www.umd.edu/catalog"]{Undergraduate Catalog}.

If you experience difficulty during the semester keeping up with the
academic demands of your courses, you may consider contacting the
Learning Assistance Service in 2201 Shoemaker Building at (301)
314-7693. Their educational counselors can help with time management
issues, reading, note-taking, and exam preparation skills.

@section{Students with Disabilities}

Students with disabilities who have been certified by Disability
Support Services as needing any type of special accommodations should
see the instructor as soon as possible during the schedule adjustment
period (the first two weeks of class). Please provide DSS's letter of
accommodation to the instructor at that time.

All arrangements for exam accommodations as a result of disability
@bold{must} be made and arranged with the instructor @bold{at least}
three business days prior to the exam date; later requests (including
retroactive ones) will be refused.

@section{Academic Integrity}

The Campus Senate has adopted a policy asking students to include the
following statement on each examination or assignment in every course:
"I pledge on my honor that I have not given or received any
unauthorized assistance on this examination (or assignment)."
Consequently, you will be requested to include this pledge on each
exam and project. Please also carefully read the Office of Information
Technology's @link["http://www.nethics.umd.edu/aup/"]{policy}
regarding acceptable use of computer accounts.

Problem sets are to be written @bold{solely with your assigned
partner}, therefore cooperation with others or use of unauthorized
materials on problem sets is a violation of the University's Code of
Academic Integrity. Both the person receiving assistance @bold{and the
person providing assistance} are in violation of the honor
code. @bold{Any evidence} of this, or of unacceptable use of computer
accounts, use of unauthorized materials or cooperation on exams or
quizzes, or other possible violations of the Honor Code, @bold{will be
submitted} to the Student Honor Council, which could result in an XF
for the course, suspension, or expulsion.

@itemlist[

@item{For learning the course concepts, students are welcome to study
together or to receive help from anyone else. You may discuss with
others the problem set requirements, the features of the programming
languages used, what was discussed in class and in the class web
forum, and general syntax errors. Examples of questions that would be
allowed are "Does a cond expression always end with an else-clause?"
or "What does a 'mismatched parenthesis' error indicate?", because
they convey no information about the contents of a problem set.}

@item{When it comes to actually writing a project assignment, other
than help from the instructional staff a project must solely and
entirely be your and your partner's own work. Working with another
student or individual, or using anyone else's work @bold{in any way}
except as noted in this paragraph, is a violation of the code of
academic integrity and @bold{will be reported} to the Honor
Council. You may not discuss design of any part of a problem set with
anyone except the instructor, teaching assistants, and your assigned
partner @bold{for that problem set}. Examples of questions you may
@bold{not} ask others might be "How did you implement this part of the
problem set?"  or "Please look at my code and help me find my stupid
syntax error!". You may not use any disallowed source of information
in creating either their project design or code. When writing projects
you are free to use ideas or @bold{short fragments} of code from
@bold{published} textbooks or @bold{publicly available} information,
but the specific source must be cited in a comment in the relevant
section of the program. }

]

@bold{Violations of the Code of Academic Integrity may include, but
are not limited to:}

@itemlist[

@item{Failing to do all or any of the work on a project by yourself,
    other than assistance from the instructional staff.}

@item{Using any ideas or any part of another person's project, or copying any other individual's work in any way.}

@item{Giving any parts or ideas from your project, including test
data, to another student.}

@item{Allowing any other students access to your program on any
computer system.}

@item{Posting solutions to your projects to publicly-accessible sites,
e.g., on github.}

@item{Transferring any part of a problem set to or from another
student or individual by any means, electronic or otherwise.}]

If you have any question about a particular situation or source then
consult with the instructors in advance. Should you have difficulty
with a programming assignment you should @bold{see the instructional
staff in office hours}, and not solicit help from anyone else in
violation of these rules.

@bold{It is the responsibility, under the honor policy, of anyone who
suspects an incident of academic dishonesty has occurred to report it
to their instructor, or directly to the Honor Council.}

Every semester the department has discovered a number of students
attempting to cheat on assignments, in violation of academic integrity
requirements. Students' academic careers have been significantly
affected by a decision to cheat. Think about whether you want to join
them before contemplating cheating, or before helping a friend to
cheat.

You are welcome and encouraged to study and compare or discuss their
implementations of the problem sets with any others after they are
graded, @bold{provided that} all of the students in question have
received nonzero scores for that assignment, and if that assignment
will not be extended upon in a later assignment.

@section{Course Evaluations}

If you have a suggestion for improving this class, don't hesitate to
tell the instructor or TAs during the semester. At the end of the
semester, please don't forget to provide your feedback using the
campus-wide CourseEvalUM system. Your comments will help make this
class better.