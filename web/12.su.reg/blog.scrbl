#lang scribble/manual
@(require "../unnumbered.rkt" "../utils.rkt")

@title*{Blog}

@section*{How to test exceptions}
@tt{Tue May 29 00:19:38 EDT 2012}

Here is a short example of how to test the exceptional behavior of a
program.

@verbatim|{
import tester.*;

// An example of how to test for exceptional behavior

// Represents a function A -> B
interface Function<A,B> {
    // Apply this function to given argument.
    B apply(A a);
}

// Represents inversion operation 1/x
class Invert implements Function<Double,Double> {
    Invert() {}

    // Produce 1/x
    public Double apply(Double x) {
	if (x.equals(0.0)) {
	    throw new RuntimeException("Divide by zero.");
	} else {
	    return 1/x;
	}
    }
}

class Examples {
    void testInv(Tester t) {
	t.checkException(// The expected exception
			 new RuntimeException("Divide by zero."),
			 // The object that throws
			 new Invert(),
			 // The method (as a string!) that throws
			 "apply",
			 // The arguments to the method
			 0.0);
    }
}
}|

@section*{Homework due is till Tuesday at midnight}
@tt{Mon May 28 22:21:58 EDT 2012}

There was a typo on @seclink["assign04"]{assignment 4} giving a due
date of 5/27, which was @bold{not} correct.  The assignment is due
Tuesday, 5/29 at midnight and the assignment has been corrected.

@section*{Solution to today's exam}
@tt{Thu May 24 11:35:08 EDT 2012}

Here is a @link["Exam3.java"]{solution}, in the form of running Java
code, to today's @link["exam3.su.12.pdf"]{exam}.


@section*{Today's lab is cancelled}
@tt{Thu May 24 10:25:07 EDT 2012}

Today's lab is cancelled.  You should spend the this time working with
your partner on the current homework assignment.


@section*{Code on safe casting, double dispatch, and instanceof}
@tt{Wed May 23 14:07:06 EDT 2012}

Here is the code from class today that shows the
@link["05.23.12.a.java"]{safe casting} approach to structural
equality, the @link["05.23.12.b.java"]{double dispatch} approach, and
finally the @link["05.23.12.c.java"]{perils of @tt{instanceof} and
casts}, which should be avoided at all costs.

@section*{Code from today on exceptions and visitors}
@tt{Tue May 22 11:42:54 EDT 2012}

Here is the @link["05.22.java"]{code} from today's class on
exceptional behavior and the visitor pattern.

@section*{Solution for assignment 2}
@tt{Tue May 22 00:17:29 EDT 2012}

@seclink["assign02"]{Assignment 2} should be graded by the end of this
week, but in the meantime, solutions to @link["2.1.java"]{2.1},
@link["2.2.java"]{2.2}, and @link["2.3.java"]{2.3} are available.

@section*{Code from today}
@tt{Mon May 21 15:17:33 EDT 2012}

Here is the @link["05.21.2012.java"]{code} from today's lecture on
sorting with comparison functions and using folds.


@section*{Bug in homework, change for Ian's office hours}
@tt{Mon May 21 09:41:58 EDT 2012}

There is a bug in @emph{How to Design Classes} that affects the
current homework.  In exercise 12.3 it says, ``Design an extension for
the classes in figures 47 and 48 that deals with equilateral right
triangles.''  After a moment's thought, you'll realize there is no
such thing as an equilateral right triangle.  Instead you should
design an extension for a isoscoles right triangle.

Ian Johnson has shifted his office hours back by a half an hour.  His
office hours are now held Friday from 1:30-3:30pm as noted on the
@seclink{General} page.

@section*{Grammatikos office hours, Assignment 3 finalized}
@tt{Thu May 17 09:37:30 EDT 2012}

James Grammatikos has finalized his @seclink["General"]{office hours},
which will be held between class and lab on Tuesday, i.e., 11:30am to
1:30pm.

@seclink["assign03"]{Assignment 3} has been finalized by adding some
book problems.


@section*{Today's code}
@tt{Tue May 15 12:50:39 EDT 2012}

Here is today's Java @link["05.15.2012.java"]{code} from class.  Also,
remember to think about how to translate our accumulator design of
@tt{reverse} from ISL to Java:

@verbatim{
;; Reverse the given list of integers
;; LoI -> LoI
(define (my-reverse lon)
  (reverse/acc lon empty))

;; LoI LoI -> LoI
;; ACCUM: rev represents list of elements we've seen so far, in reverse order.
(define (reverse/acc lon rev)
  (cond [(empty? lon) rev]
        [(cons? lon)
         (reverse/acc (rest lon)
                      (cons (first lon) rev))]))
}

@section*{No need to interleave}
@tt{Tue May 15 09:41:02 EDT 2012}

The @tt{interleave} problem of the current assignment requires you to
use concepts we haven't covered in class.  You should try to solve the
problem and see what goes wrong, but the @tt{interleave} part of the
assignment will not be graded.


@section*{Two new 2510 tutors and CCIS tutors}
@tt{Mon May 14 09:01:51 EDT 2012}

We've added James Grammatikos and Jason Shrand to the course staff as
tutors.  James and Jason will be holding office hours throughout the
week and are here to help you.  Be sure to visit them.  See the
@secref{General} page for details.

In addition to the dedicated 2510 tutors, there are CCIS tutors
available that can also help.  There schedule is:
@itemlist[
@item{Monday 5:15-7:15pm Dan King, 102WVH}
@item{Wednesday: 1-3pm Stephanie Lund, 102WVH}]

@section*{Assignment 2 is up}
@tt{Thu May 10 20:32:12 EDT 2012}

@seclink["assign02"]{Assignment 2} is finally up.  It's due date has
been moved back to Tuesday at midnight so that we can show you how to
use @tt{svn} to submit your homework in lab on Tuesday.


@section*{Homework partners}
@tt{Wed May  9 17:41:37 EDT 2012}

Here are the initial partner assignments.  If something isn't right,
email @tt{dvanhorn} immediately.

@verbatim|{
pair01: acalo joejeno
pair02: kevin08 kmiao
pair03: sripley7 ajacks
pair04: merylas bjliuy
pair05: michng goodwin
pair06: guom dsimond
pair07: jcaron jwall
pair08: deema psanshi
pair09: schwarta gabriel
pair10: jbrooks2 ascherj
pair11: bmccabe4 jkosof
pair12: cungtran gudosha
pair13: yusuf.m nnamdi93
pair14: jinwow gudosha
}|


@section*{Tester library}
@tt{Wed May  9 08:56:37 EDT 2012}

We will use the @link["http://www.ccs.neu.edu/javalib/Tester/"]{Tester
Library} developed here at NEU by Prof. Proulx in order to test our
Java programs.

To use the library, you'll need to download the tester
@link["http://www.ccs.neu.edu/javalib/Downloads/v1.4.1/tester1_4_1.jar"]{jar
file}.


@section*{Past exams}
@tt{Wed May  9 00:59:17 EDT 2012}


Here is a complete set of recent exams from Fundamentals II.  Much of
this material is not relevant for Thursday exam, since it will only
cover what we have studied in class, lab, homeworks, and course
readings so far.  However, some of these problems may prove useful in
preparing for Thursday's exam.  The remaining material will become
more useful as the semester goes on.

@itemlist[
@item{Exam 1: @link["exam1-sol-sp10.pdf"]{Spring 2010 (A)}.}
@item{Exam 1: @link["exam1v2-sol-sp.pdf"]{Spring 2010 (B)}.}
@item{Exam 1: @link["exam1-sol-su10.pdf"]{Summer 2010}.}
@item{Exam 1: @link["exam1-sol-fl10.pdf"]{Fall 2010}.}
@item{Exam 1: @link["exam1a.pdf"]{Spring 2012 (A)}.}
@item{Exam 1: @link["exam1bx.pdf"]{Spring 2012 (B)}.}

@item{Exam 2: @link["exam2-su10-sol.pdf"]{Summer 2010}.}
@item{Exam 2: @link["exam2-v1-sp10.pdf"]{Spring 2010 (A)}.}
@item{Exam 2: @link["exam2-v2-sp10.pdf"]{Spring 2010 (B)}.}
@item{Exam 2: @link["exam2-fl10-sol.pdf"]{Fall 2010}.}
@item{Exam 2: @link["exam2-sp11.pdf"]{Spring 2011}.}
@item{Exam 2: @link["exam2a.pdf"]{Spring 2012 (A)}.}
@item{Exam 2: @link["exam2b.pdf"]{Spring 2012 (B)}.}
@item{Exam 2: @link["exam2H-sp12.pdf"]{Spring 2012 (H)}.}

@item{Exam 3: @link["exam3-fl09-sol.pdf"]{Fall 2009}.}
@item{Exam 3: @link["exam3-sp10-no-sol.pdf"]{Spring 2010}.}
@item{Exam 3: @link["exam3-fl10-no-sol.pdf"]{Fall 2010}.}
@item{Exam 3: @link["exam3-sp11-sol.pdf"]{Spring 2011}.}
@item{Exam 3: @link["exam3.pdf"]{Spring 2012}.}
]

@section*{Ian Johnson}
@tt{Tue May  8 09:39:07 EDT 2012}

Ian Johnson has signed on as the head TA for our course.  Ian is a PhD
student in programming languages and does hairy research on automated
reasoning about programs with Profs. Van Horn and Shivers.

Check out the @secref{General} page for his office hours.

@section*{Don't have a CCIS username?}
@tt{Mon May  7 13:21:56 EDT 2012}

If you do not have a CCIS account, you should see the Systems office
on the third floor of WVH @emph{today!}  It takes two days to process
the request so you may not have an account by the time tonight's
homework is due.  If that's the case, please submit the CCIS username
you requested and indicate in your submission that the account is
still pending.


@section*{Code from today's lecture}
@tt{Mon May  7 13:21:56 EDT 2012}

Here is the @link["5-7.rkt"]{code} from today's lecture.

@section*{Welcome to CS2510}
@tt{Sun May  6 23:04:12 EDT 2012}

We hope you'll have fun.

@subsection*{Programming is like Cooking}

Here's a recent NYTimes article
(@link["http://www.ccs.neu.edu/home/dvanhorn/tmp/programming-is-like-cooking.pdf"]{There's
the Wrong Way and Jacques Pépin's Way}) about the chef Jacques Pépin,
author of @emph{La Technique}.  There are many fruitful analogies
between programming and cooking; one of which is that Pépin's
"technique" is very much like our design recipe.  The goal of
Fundamentals I and II is to instill the essential technique of great
programmers, and to borrow Pépin's words, "Once you learn the
technique, then can be a creative programmer; a great programmer is
first a tehnician."  And there's only one way to become a master of
technique: "you have to repeat, repeat, repeat, repeat until it
becomes part of yourself."


@;{
@section*{Project presentation info}
@tt{Wed Apr 18 00:57:26 EDT 2012}

Here some @link["ProjectPresentations.html"]{info} on project
presentations.


@section*{Code from Review Session}
@tt{Tue Apr 10 09:22:33 EDT 2012}

Here is the @link["Review.zip"]{code} from last night's review
session.  The instructors make @emph{no claim of correctness} of this
code.

@section*{Review session time and location}
@tt{Sat Apr  7 11:00:21 EDT 2012}

The review session for exam 3 will be held Monday 4/9 from 6-8pm in
135 Shillman Hall and will be led by Nikko Patten and Scott Lindeman.
Come prepared with questions.


@section*{Assignment 13 posted}
@tt{Thu Apr  5 12:50:11 EDT 2012}

@link["Assignment13.html"]{Assignment 13}, the final project, has now
been posted.  Start working on it early because it is a large project.


@section*{Another practice exam}
@tt{Thu Apr  5 12:11:06 EDT 2012}

Here is the @link["exam2H-sp12.pdf"]{honors exam 2} from this
semester, which may also be useful in preparing for this class's exam
3.


@section*{Exam location}
@tt{Thu Apr  5 12:01:35 EDT 2012}

The exam will be held in 20 West Village F.


@section*{Past exams}
@tt{Thu Apr  5 08:57:07 EDT 2012}

Don't forget: there is an out-of-class exam from 6-9pm on Tuesday
4/10.  (Be sure to eat dinner before arriving!)

Here are the past exams from @link["exam3-fl09-sol.pdf"]{Fall 2009},
@link["exam3-fl10-no-sol.pdf"]{Fall 2010},
@link["exam3-sp10-sol.pdf"]{Spring 2010},
@link["exam3-sp11-sol.pdf"]{Spring 2011}, and
@link["exam3-su10.pdf"]{Summer 2010}.


@section*{Macbeth.txt}
@tt{Fri Mar 30 10:37:30 EDT 2012}

The current homework mentions but does not provide a
@link["Macbeth.txt"]{Macbeth.txt}, which is now available.


@section*{5 Under 25; Tonight}
@tt{Thu Mar 22 08:31:56 EDT 2012}

Here is an email from Fundies II alum Mike Amirault about tonight's 5
Under 25 event.  Hope you can make it.

@;image[#:scale .1 #:suffixes '(".png")]{CCISFlyer.jpg}

Hello Prof. Van Horn,

My name is Mike Amirault and I am a member of the Student Alumni
Association (SAA) and also am in the class of 2013 here in CCIS. I was
in your Fundies 2 class way back in the spring of 2009 and also have
tutored Fundies 1 for you the following year.  I wanted to take the
opportunity to inform you of an upcoming networking series that the
Student Alumni Association and the Office of Alumni Relations hosts
that will be beneficial for your students.

The program is called 5 Under 25, and it is a panel series that gives
current students a glimpse of where their education and experiences at
Northeastern can take them. The 5 panelists are all recent CCIS
graduates who are under 25 years old. The program gives students the
opportunity to ask questions and hear about co-op experiences, class
schedules, student involvement, job searches and their emerging
careers.

This month, the Student Alumni Association will be hosting 5 Under 25
for the College of Computer & Information Science on Thursday, March
22nd at 6:30pm in the Alumni Center (716 Columbus Ave, 6th floor,
across from Squashbusters). There will be free pizza, drinks and
dessert served.

The panel will include :

Brian Correia – Technology Support Specialist

Dan Gonyea – Software Development Engineer

Lauren Siegert – Product Software Engineer

Phil Kelly - Systems Administrator

Jeff Cumplido - TBA

I would greatly appreciate it if you could let your CCIS students know
of this event so they may have the opportunity to hear alumni success
stories and make key contacts. If you have any questions about the
program, please contact me at amirault.m at husky.neu.edu.

Best,

Mike

What: 5 Under 25, College of Computer & Information Sciences
Where: Alumni Center, 6th floor, 716 Columbus Ave (across from Squash Busters)
When: Thursday, March 22nd at 6:30pm
Who: All students interested in networking with successful young alumni and learning more information
about the CCIS classes, student groups, co-ops, careers and more.



@section*{New Homework partners}
@tt{Fri Mar 16 12:37:13 EDT 2012}

Here are the new homework partner assignments, which are now in effect:

@verbatim{
112	patelv99 klebsh
113	bswords kvinagro
114	nathanl patchca
115	seebert akaz
116	cpaszul zlyford
117	benuhl sking19
118	evannoyz dc93
119	shryock jkreell
120	mhill butlerch
121	riley kandebo
122	eromeiro lyaunzbe
123	mclamp thu
124	obrienjj byoshi92
125	mostrow luoyj
126	cookid conorao
127	strandl dciatto
128	adamwigs kkotosky
129	mccoy dforman
130	lmarelic kbriskin
131	elisef lahirud
132	chlacher vkonen
133	dohertyc rysull
134	agimmi00 iboehman
135	gary2546 maggiec
136	jbrooks2 lind
137	jordanre olinaba
138	tmacieik jnanni
139	sasia hartmand
140	hlavin patcinc
141	ntinsley ascherj
142	andrewm billyguo
143	kohlerjp bfaller
144	tedlin swazi
145	mckinnon eabraham
146	seydi pburden
147	ggoode slynch
148	slindber cmcelear
149	ascholl ferland
150	tylere soelgary
151	mhmdfy albeht0a
152	rimipat5 thurston
153	rstrass ravert1
154	marcs jfahy
155	spatel91 lopeza
156	pietdan johndrew
157	aedgett shivam
158	cyvuong mrodrig
159	slee toder13
160	cmentzer hamuchea
161	jacobsr afasulo
162	millarj bmccabe4
163	alexj kleinjm
164	joshs abarba
165	emichel emilytx
}


@section*{Homework extension due to power outage}
@tt{Wed Mar 14 18:58:06 EDT 2012}

The deadline for the current homework assignment as been extended for
an additional 24 hours due to the power outage.


@section*{Room for exam review}
@tt{Mon Mar 12 08:56:42 EDT 2012}

The room for the exam review is @bold{108SN} (not 10BK as previously
noted).  It is Tuesday 3/13 from 6-9pm.


@section*{Cleaned up BT code from lecture}
@tt{Thu Mar  1 08:45:34 EST 2012}

@link["bt.zip"]{Here} is the cleaned up binary tree code from last
lecture.

@section*{Homework up, past exams, exam review}
@tt{Wed Feb 29 23:45:57 EST 2012}

The current assignment is @link["Assignment8.html"]{posted} and is due
(with your current partners) on the Wednesday after break, 3/14.

Here are some past exam 2s from 
@link["exam2-v1-sp10.pdf"]{Spring 2010 (A)} and
@link["exam2-v2-sp10.pdf"]{(B)},
@link["exam2-su10-sol.pdf"]{Summer 2010}, 
@link["exam2-fl10-sol.pdf"]{Fall
2010}, and @link["exam2-sp11.pdf"]{Spring 2011}.
Note that not all of the exams include solutions.

There will be an exam review session Tuesday 3/13 currently scheduled
for @bold{108SN} (corrected) from 6-9pm.

@section*{Global constants}
@tt{Wed Feb 22 08:26:11 EST 2012}

Here are some @link["GlobalConstants.html"]{notes} on how to use
global constants.


@section*{Note on problem 2, part 6}
@tt{Mon Feb 20 15:03:46 EST 2012}

Note: You may want to build a BST from the nodes in the given list.


@section*{Marathon code}
@tt{Mon Feb 13 11:19:36 EST 2012}

Here is the @link["Marathon.java"]{code} we've developed so far for
the Boston Marathon.

Here are a couple things worth trying to do:

@itemlist[
  @item{Sort the list of runners in alphabetic order.}
  @item{Compare the code for @tt{all} and @tt{any} to @tt{andmap} and @tt{ormap}
        from last semester.}
  @item{Try to develop the Java analogue of @tt{filter}.}
  @item{More challenging: develop @tt{winner} which produces the
  runner with the best time.  (What should you do if there are no runners?)}
  @item{Develop a variation on @tt{winner} that produces the runner
  with the lowest bib number.}
]

	


@section*{Current homework partners}
@tt{Sat Feb 11 13:17:27 EST 2012}

Here are the current homework partners:

@verbatim{
[cs2510spring2012:/pair055]
iboehman = rw
agimmi00 = rw

[cs2510spring2012:/pair056]
luoyj = rw
mostrow = rw

[cs2510spring2012:/pair057]
tedlin = rw
evannoyz = rw

[cs2510spring2012:/pair058]
mhmdfy = rw
albeht0a = rw

[cs2510spring2012:/pair059]
lahirud = rw
spatel91 = rw

[cs2510spring2012:/pair060]
jacobsr = rw
vkonen = rw

[cs2510spring2012:/pair061]
dforman = rw
mclamp = rw

[cs2510spring2012:/pair062]
rstrass = rw
billyguo = rw

[cs2510spring2012:/pair063]
kleinjam = rw
alexj = rw

[cs2510spring2012:/pair064]
nathanl = rw
eromeiro = rw

[cs2510spring2012:/pair065]
millarj = rw
lmarelic = rw

[cs2510spring2012:/pair066]
ggoode = rw
slynch = rw

[cs2510spring2012:/pair067]
bmccabe4 = rw
kbriskin = rw

[cs2510spring2012:/pair068]
byoshi92 = rw
johndrew = rw

[cs2510spring2012:/pair069]
lyaunzbe = rw
kohlerjp = rw

[cs2510spring2012:/pair070]
aedgett = rw
kvinagro = rw

[cs2510spring2012:/pair071]
lahirud = rw
spatel91 = rw

[cs2510spring2012:/pair072]
kkotosky = rw
adamwigs = rw

[cs2510spring2012:/pair073]
klebsh = rw
eabraham = rw

[cs2510spring2012:/pair074]
patchca = rw
conorao = rw

[cs2510spring2012:/pair075]
hlavin = rw
obrienjj = rw

[cs2510spring2012:/pair076]
mhill = rw
butlerch = rw

[cs2510spring2012:/pair077]
tmacieik = rw
dc93 = rw

[cs2510spring2012:/pair078]
benuhl = rw
pietdan = rw

[cs2510spring2012:/pair079]
seydi = rw
riley = rw

[cs2510spring2012:/pair080]
emilytx = rw
ferland = rw

[cs2510spring2012:/pair081]
slee = rw
toder13 = rw

[cs2510spring2012:/pair082]
bswords = rw
ascherj = rw

[cs2510spring2012:/pair083]
sking19 = rw
ascholl = rw

[cs2510spring2012:/pair084]
ntinsley = rw
thurston = rw

[cs2510spring2012:/pair085]
pburden = rw
kandebo = rw

[cs2510spring2012:/pair086]
patelv99 = rw
tylere = rw

[cs2510spring2012:/pair087]
cyvuong = rw
soelgary = rw

[cs2510spring2012:/pair088]
ravert1 = rw
mrodrig = rw

[cs2510spring2012:/pair089]
joshs = rw
abarba = rw

[cs2510spring2012:/pair090]
slindber = rw
rysull = rw

[cs2510spring2012:/pair091]
cpaszul = rw
andrewm = rw

[cs2510spring2012:/pair092]
seebert = rw
elisef = rw

[cs2510spring2012:/pair093]
shryock = rw
mccoy = rw

[cs2510spring2012:/pair094]
thu = rw
hartmand = rw

[cs2510spring2012:/pair095]
swazi = rw
jkreell = rw

[cs2510spring2012:/pair096]
lind = rw
jfahy = rw

[cs2510spring2012:/pair097]
lopeza = rw
akaz = rw

[cs2510spring2012:/pair098]
jordanre = rw
cmcelear = rw

[cs2510spring2012:/pair099]
marcs = rw
cookid = rw

[cs2510spring2012:/pair100]
hamuchea = rw
chlacher = rw

[cs2510spring2012:/pair101]
mckinnon = rw
dohertyc = rw

[cs2510spring2012:/pair102]
rimipat5 = rw
sm0ss117 = rw

[cs2510spring2012:/pair103]
dciatto = rw
kleinjm = rw

[cs2510spring2012:/pair104]
jbrooks2 = rw
shivam = rw

[cs2510spring2012:/pair105]
bfaller = rw
strandl = rw

[cs2510spring2012:/pair106]
jnanni = rw
cmentzer = rw

[cs2510spring2012:/pair107]
gary2546 = rw
maggiec = rw

[cs2510spring2012:/pair108]
patcinc = rw
sasia = rw

[cs2510spring2012:/pair109]
afasulo = rw
zlyford = rw

[cs2510spring2012:/pair110]
olinaba = rw
kaelanc = rw
}


@section*{Past exams}
@tt{Thu Feb  2 20:28:08 EST 2012}

Here are the past exams:
@itemlist[
@item{@link["exam1-sol-sp10.pdf"]{Exam 1, Spring 2010 (Version 1)}}
@item{@link["exam1v2-sol-sp10.pdf"]{Exam 1, Spring 2010 (Version 2)}}
@item{@link["exam1-sol-su10.pdf"]{Exam 1, Summer 2010}}
@item{@link["exam1-sol-fl10.pdf"]{Exam 1, Fall 2010}}
]

@section*{Room for Exam Review: 201MU}
@tt{Thu Feb  2 11:57:24 EST 2012}

The exam review will be in
@link["http://www.northeastern.edu/campusmap/map/qad5.html"]{201 Mugar
Life Sciences Building (MU)} from 6pm-8pm on 2/7.  Be sure to come
prepared with questions for Nikko and Scott.

@section*{Question on homehork}
@tt{Thu Feb  2 08:07:50 EST 2012}

Here is a question we got on the current homework:

@indented{
I am currently working on completing the @tt{directionTo} method in
the second problem of the homework, but was not sure what the method
should return if the two points are the same.  Should I assume that
they're always different points, or should it return @racket["Same Position"], 
or maybe even throw an exception?  I figured I would ask
since the problem didn't really explain that particular scenario and I
just wanted to make sure that I implemented the method properly.}

Good question.

When the customer does not specify something it is Ok for the
programmer to design a reasonable solution and document the choices
he/she has made.

I think returning the String @racket["Same Position"] sounds
reasonable, especially considering that as of this homework we know
nothing about exceptions, and that we really do not want the program
to crash, just because for a short while during our trip we take a
break for lunch and stay in the same place.


@section*{Bookstore code}
@tt{Tue Jan 31 17:21:51 EST 2012}

Here is the worked out @link["BookstoreAbstract.java"]{bookstore code}
from class.

@section*{Shark code from last lecture}
@tt{Mon Jan 23 08:00:46 EST 2012}

Here's the @link["Shark.java"]{shark code} we've been developing in
class.

@section*{Small revisions to Assignment 2, SVN guide, Code style}
@tt{Fri Jan 20 15:41:20 EST 2012}

We've made some small adjustments to
@link["Assignment2.html"]{assignment 2}, so be sure to read the latest
version.  (You should reload the page in your browser if you've
visited it recently.)

Jonathan Schuster has put together a nice
@seclink["Subversion"]{guide} on using Subversion that includes
instructions for Windows, Macs, and Linux machines.  It also tells you
how to organize your code and how to set up Eclipse to work with the
repository.

We've also added a note on @seclink["Code_style"]{proper code style}
that you should use for all the code you write in this class.
}