#lang scribble/manual
@(require scribble/core racket/list)

@title[#:style 'unnumbered]{Schedule}


@section{At a glance}

There are three weeks of class.

Every class consists of lecture and lab.

There are exams at the beginning of class every Friday.

There are assignments due at the end of every Friday.

There are exercises due every weekday except Friday.

There are daily surveys and quizes.

There are daily readings from the HtDP book and the documentation.

@section{Week 1}

@(define (d s) (nonbreaking (bold s)))

@(define (htdp part name)
   (elem "HtDP: "
     (link (string-append "https://htdp.org/2018-01-06/Book/part_" part) name)))

@(define (docs lang name)
  (elem "Docs: "
    (secref name
            #:tag-prefixes (list lang)
            #:doc '(lib "scribblings/htdp-langs/htdp-langs.scrbl"))))


@(define (week-table entries)
  @tabular[
    #:style 'boxed 
    #:sep @hspace[1]
    #:row-properties '(bottom-border)
    (cons (list @bold{Date} @bold{Topic} @bold{Reading})
          entries)])
  
@week-table[
 (list (list
        @d{July 8} 
        @itemlist[
          @item{Introduction; course overview}
          @item{Arithmetic of numbers and images}
          @item{Functions and programs}
          @item{@secref{lab1}}
          @item{Due: @secref{ex1}}]
        @itemlist[
          @item{@htdp["preface.html"]{Preface}, through "Skills that Transfer"}
          @item{@htdp["one.html#%28part._ch~3abasic-arithmetic%29"]{Arithmetic}}
          @item{@htdp["one.html#%28part._ch~3afuncs-progs%29"]{Functions and Programs}}
          @item{@docs["htdp-beginner" "Numbers__Integers__Rationals__Reals__Complex__Exacts__Inexacts"]}
          @item{@docs["htdp-beginner" "Booleans"]}
          @item{@docs["htdp-beginner" "Strings"]}])       
       (list
        @d{July 9}
        @itemlist[
          @item{Animations}
          @item{Design recipe}
          @item{Intervals, enumerations, and itemizations}
          @item{Due: @secref{ex2}}]
        @itemlist[@item{@htdp["one.html#(part._ch~3ahtdp)"]{How to Design Programs}}
                  @item{@htdp["one.html#(part._ch~3aintervals-enums)"]{Intervals, Enumerations, and Itemizations}}
                  @item{Docs: @link["https://docs.racket-lang.org/teachpack/2htdpimage-guide.html"]{Image guide}}])
       (list
        @d{July 10} 
        @itemlist[
          @item{Structures}
          @item{Itemizations, revisited}
          @item{Due: @secref{ex3}}]
        @itemlist[
          @item{@htdp["one.html#(part._ch~3astructure)"]{Adding Structure}}                                
          @item{@htdp["one.html#(part._ch~3amix)"]{Itemizations and Structures}}
          @item{@htdp["one.html#(part._ch~3asummary1)"]{Summary (Part I)}}
          @item{@docs["htdp-beginner" "Posns"]}])
       (list
        @d{July 11}
        @itemlist[
          @item{World programs}
          @item{Due: @secref{ex4}}]
        @itemlist[@item{Docs: @link["https://docs.racket-lang.org/teachpack/2htdpuniverse.html"]{Worlds}}
                  
                  ])
       (list
        @d{July 12}
        @itemlist[@item{Midterm 1}
                  @item{Lists}
                  @item{Due: @secref{assign1}}]
        @itemlist[@item{@htdp["two.html#(part._ch~3alists1)"]{Lists}}]))]

@section{Week 2}

@week-table[
(list
        (list
         @d{July 15}
         @itemlist[
           @item{Moar lists}
           @item{Self-referential Data}
           @item{Due: @secref{ex5}}]
         @itemlist[@item{@htdp["two.html#(part._ch~3adesign-lists)"]{Designing with Self-Referential Data Definitions}}
                  @item{@htdp["two.html#(part._ch~3alists2)"]{More on Lists}}])

        (list @d{July 16}
              @itemlist[
                @item{Composition}
                @item{Abstraction}
                @item{Due: @secref{ex6}}]
              @itemlist[
                @item{@htdp["two.html#(part._ch~3alist-sort)"]{Design by Composition}}
                @item{@htdp["two.html#(part._ch~3asummary2)"]{Summary (Part II)}}
                @item{@htdp["three.html#(part._ch~3add-similarities)"]{Similarities Everywhere}}])                   

        (list @d{July 17} 
              @itemlist[
                @item{Designing abstractions}
                @item{Using abstractions}
                @item{Due: @secref{ex7}}]
              @itemlist[
                   @item{@htdp["three.html#(part._ch~3aabstract)"]{Designing Abstractions}}
                   @item{@htdp["three.html#(part._ch~3a3use)"]{Using Abstractions}}])

        (list @d{July 18}
              @itemlist[
                @item{Lambda, lambda, lambda}
                @item{Due: @secref{ex8}}]
              @itemlist[
                @item{@htdp["three.html#(part._ch~3a3lambda)"]{Nameless Functions}}
                @item{@htdp["three.html#(part._ch~3asummary3)"]{Summary (Part III)}}])

        (list @d{July 19} 
              @itemlist[
                @item{Midterm 2}
                @item{S-Expressions}
                @item{Due: @secref{assign2}}]
              @itemlist[@item{@htdp["part_four.html#(part._ch~3apoetry-sexp)"]{The Poetry of S-expressions}}]))]

@section{Week 3}

@week-table[
(list
        (list
         @d{July 22}
         @itemlist[
           @item{Due: @secref{ex9}}]
         'cont)

        (list @d{July 23}
             @itemlist[
               @item{Due: @secref{ex10}}]
             'cont)
        (list @d{July 24} 
             @itemlist[
               @item{Due: @secref{ex11}}]
             'cont)
        (list @d{July 25} 
             @itemlist[
               @item{Due: @secref{ex12}}]
             'cont)
        (list @d{July 26} 
              @itemlist[
                @item{Final Exam} 
                @item{Perspectives}
                @item{Due: @secref{assign3}}]
              @itemlist[@item{@link["https://www.nostarch.com/realmofracket"]{Realm of Racket} (suggested)}]))
]



@;{tabular[#:style 'boxed
	 #:sep @hspace[1] 
	 #:row-properties '(bottom-border)
	 (list (list @bold{Date} @bold{Topic} @bold{Reading})
               (list @d{Aug 27} @elem{@emph{No class!}} @elem{@link["https://htdp.org/2018-01-06/Book/part_preface.html"]{Preface}, through "Skills that Transfer"})
	       (list @d{Aug 29} @elem{Introduction; course overview} @elem{@link["https://htdp.org/2018-01-06/Book/part_prologue.html"]{Prologue}})
	       (list @d{Aug 31} @elem{Arithmetic} @elem{@link["https://htdp.org/2018-01-06/Book/part_one.html#%28part._ch~3abasic-arithmetic%29"]{Arithmetic}})
	       (list @d{Sep 3} @elem{@emph{Labor Day, no class}} 'cont)
	       (list @d{Sep 5} @elem{Functions and Programs} )
	       (list @d{Sep 7} @elem{The Recipe} @elem{@link["https://htdp.org/2018-01-06/Book/part_one.html#%28part._ch~3ahtdp%29"]{How to Design Programs}})
	       (list @d{Sep 10} @elem{Designing Worlds} 'cont)
	       (list @d{Sep 12} @elem{Intervals, Enumerations, Itemizations} @elem{@link["https://htdp.org/2018-01-06/Book/part_one.html#%28part._ch~3aintervals-enums%29"]{Intervals, Enumerations, and Itemizations}})
	       (list @d{Sep 14} @elem{Structures} @elem{@link["https://htdp.org/2018-01-06/Book/part_one.html#%28part._ch~3astructure%29"]{Adding Structure}})
	       (list @d{Sep 17} @elem{Itemizations and Structures} @elem{@link["https://htdp.org/2018-01-06/Book/part_one.html#%28part._ch~3amix%29"]{Itemizations and Structures} and @link["https://htdp.org/2018-01-06/Book/part_one.html#%28part._ch~3asummary1%29"]{Summary}})
	       (list @d{Sep 19} @elem{Lists} @elem{@link["https://htdp.org/2018-01-06/Book/i1-2.html"]{Intermezzo 1: BSL} and @link["https://htdp.org/2018-01-06/Book/part_two.html#%28part._ch~3alists1%29"]{Lists}})
	       (list @d{Sep 21} @elem{Self-referential Data Definitions} @elem{@link["https://htdp.org/2018-01-06/Book/part_two.html#%28part._ch~3adesign-lists%29"]{Designing with Self-Referential Data Definitions}})
	       (list @d{Sep 24} @elem{More Lists} @elem{@link["https://htdp.org/2018-01-06/Book/part_two.html#%28part._ch~3alists2%29"]{More on Lists}})
	       (list @d{Sep 26} @elem{Design by Composition} @elem{@link["https://htdp.org/2018-01-06/Book/part_two.html#%28part._ch~3alist-sort%29"]{Design by Composition}})
	       (list @d{Sep 28} @elem{Midterm review} 'cont)
	       (list @d{Oct 1} @elem{@bold{Midterm 1}} 'cont)
	       (list @d{Oct 3} @elem{Design by Composition} @elem{@link["https://htdp.org/2018-01-06/Book/part_two.html#%28part._ch~3aproj-lists%29"]{Project: Lists} and @link["https://htdp.org/2018-01-06/Book/part_two.html#%28part._ch~3asummary2%29"]{Summary}})
	       (list @d{Oct 5} @elem{The Snake Game, Part I} @elem{@link["https://htdp.org/2018-01-06/Book/part_three.html"]{Abstraction} and @link["https://htdp.org/2018-01-06/Book/part_three.html#%28part._ch~3add-similarities%29"]{Similarities Everywhere}})
	       (list @d{Oct 8} @elem{The Snake Game, Part II} @elem{@link["https://htdp.org/2018-01-06/Book/part_three.html#%28part._ch~3aabstract%29"]{Designing Abstractions}})
	       (list @d{Oct 10} @elem{Highlights} @elem{@link["https://htdp.org/2018-01-06/Book/part_three.html#%28part._ch~3a3use%29"]{Using Abstractions}})
	       (list @d{Oct 12} @elem{Designing Abstractions, Part I} @elem{@link["https://htdp.org/2018-01-06/Book/part_three.html#%28part._ch~3a3lambda%29"]{Nameless Functions} and @link["https://htdp.org/2018-01-06/Book/part_three.html#%28part._ch~3asummary3%29"]{Summary}})
	       (list @d{Oct 15} @elem{Designing Abstractions, Part II} @elem{@link["https://htdp.org/2018-01-06/Book/i3-4.html"]{Intermezzo 3: Scope and Abstraction}})
	       (list @d{Oct 17} @elem{Using Abstractions} 'cont)
	       (list @d{Oct 19} @elem{Though Shall Not Be Named} 'cont)
	       (list @d{Oct 22} @elem{Trees} @elem{@link["https://htdp.org/2018-01-06/Book/part_four.html#%28part._ch~3apoetry-sexp%29"]{The Poetry of S-Expressions}})
	       (list @d{Oct 24} @elem{Forests, S-Expressions} 'cont)
	       (list @d{Oct 26} @elem{Designing with Intertwined Data} @elem{@link["https://htdp.org/2018-01-06/Book/part_four.html#%28part._ch~3amoney-sexp%29"]{Project: The Commerce of XML}})
	       (list @d{Oct 29} @elem{Incremental Refinement} @elem{@link["https://htdp.org/2018-01-06/Book/part_four.html#%28part._ch~3afiles%29"]{Iterative Refinement}})
	       (list @d{Oct 31} @elem{Simultaneous Processing} @elem{@link["https://htdp.org/2018-01-06/Book/part_four.html#%28part._ch~3asimu%29"]{Simultaneous Processing}})
	       (list @d{Nov 2} @elem{Non-standard Recursion} @elem{@link["https://htdp.org/2018-01-06/Book/part_five.html#%28part._ch~3astrange-recursions%29"]{Non-standard Recursion}})
	       (list @d{Nov 5} @elem{@bold{Midterm 2}} 'cont)
	       (list @d{Nov 7} @elem{Midterm Retrospective} 'cont)
	       (list @d{Nov 9} @elem{@emph{No class}} 'cont)
	       (list @d{Nov 12} @elem{Designing Algorithms} @elem{@link["https://htdp.org/2018-01-06/Book/part_five.html#%28part._ch~3adesign-algo%29"]{Designing Algorithms}})
	       (list @d{Nov 14} @elem{Variations} @elem{@link["https://htdp.org/2018-01-06/Book/part_five.html#%28part._ch~3agen-rec-samples%29"]{Variations on the Theme}})
	       (list @d{Nov 16} @elem{Backtracking} @elem{@link["https://htdp.org/2018-01-06/Book/part_five.html#%28part._ch~3abacktrack%29"]{Algorithms that Backtrack}})
	       (list @d{Nov 19} @elem{Designing Accumulator-style Functions} @elem{@link["https://htdp.org/2018-01-06/Book/part_six.html"]{Accumulators and the Loss of Knowledge}})
	       (list @d{Nov 21} @elem{@emph{Thanksgiving break, no class}} 'cont)
	       (list @d{Nov 23} @elem{@emph{Thanksgiving break, no class}} 'cont)
	       (list @d{Nov 26} @elem{Accumulating More} @elem{@link["https://htdp.org/2018-01-06/Book/part_six.html#%28part._sec~3adesign-accu%29"]{Designing Accumulator-Style Functions} and @elem{@link["https://htdp.org/2018-01-06/Book/part_six.html#%28part._ch~3amore-accu%29"]{More Uses of Accumulation}}})
	       (list @d{Nov 28} @elem{Types, Part I} 'cont)
	       (list @d{Nov 30} @elem{Types, Part II} 'cont) 
	       (list @d{Dec 3} @elem{Objects, Part I} 'cont)
	       (list @d{Dec 5} @elem{Objects, Part II} 'cont)
	       (list @d{Dec 7} @elem{Conclusions and Perspective} 'cont)
	       (list @d{Dec 10} @elem{Final Review} 'cont))]



}

