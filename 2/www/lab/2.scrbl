#lang scribble/manual
@(require scribble/core (for-label lang/htdp-beginner) "helper.rkt")
@(require "../utils.rkt")

@lab-title[2]{More Data Definitions}

@section[#:style 'unnumbered #:tag "lab2:intro"]{Introduction}

You'll work in this lab in ad-hoc pairs. Find a partner and get
started.

The two of you will work as a team to solve problems. At any time, one of you
will be the @bold{Head} and the other will be the @bold{Hands}. The @bold{Head}
does the thinking and the @bold{Hands} does the typing. @bold{Hands} type only
what the @bold{Head} tells them to, but you're free to discuss any issues that
pop up. We'll have you switch off during the lab to make sure each of you get
practice problem solving, dealing with syntax, and getting finger exercises on
the keyboard.

You both should install DrRacket, but only one instance should be use during the
lab. At the end you'll submit the lab as a pair via the
@link["https://submit.cs.umd.edu"]{UMD CS Submit Server} so we can keep an eye
on lab attendance.


@section[#:tag "lab2:class"]{Fixing Install Issues}

See @labref{1} for full installation details.

The installation issues from last lab were due to whitespace being
copied into the "Package Source" text area along with the following
URL:

@centered{
@tt{https://github.com/dvanhorn/dpc.git#racket-v6.10}}

If you encounter issues during the installation be sure to delete all
text from the text area before you press "Install", @emph{even if you
do not see any text}. Call a TA over if you continue to have issues.

@section[#:tag "lab2:ex"]{Finger exercises}

In the following exercises you'll find simple programs that follow the
design recipe from last semester. Redesign these programs using the
@tt{class/0} language. You should use classes and methods instead in
place of atomic data, structures, and functions.

Each exercise builds on the last. You should solve the exercises
in-order and in the same @emph{definitions window}.

@larger{@bold{Ex 1}}: This program is a simple video player.

@isl-block{
;; A Video is a (make-video title year play-count)
;; where title      is a String,
;;       year       is an Int,
;;   and play-count is a PositiveInt.
;; Interp: A video multimedia entry.
(define-struct video (title year play-count))

;; play : Video -> Video
;; Play the given video, incrementing its play-count.
(check-expect (play-video (make-video "Caddyshack" 1980 4))
              (make-video "Caddyshack" 1980 5))
(define (play-video v)
  (make-video (video-title v) (video-year v) (add1 (video-play-count v))))
}

@larger{@bold{Ex 2}}: This program compares two videos to find the earlier.

@isl-block{
;; earlier? : Video Video -> Boolean
;; Was the first given video made earlier than the second?
(check-expect (video-earlier (make-video "Caddyshack" 1980 4)
                             (make-video "Aladdin" 1992 40))
              #true)
(define (video-earlier v1 v2)
  (< (video-year v1) (video-year v2)))
}

@larger{@bold{Ex 3}}: This program collects videos in a library,
allowing multiple videos to be played at once.

@isl-block{
;; A VideoLibrary is one of:
;; - (make-mt)
;; - (make-kons Video VideoLibrary)
;; Interp: A collection of Videos
(define-struct mt ())
(define-struct kons (first rest))

;; play-all-videos : VideoLibrary -> VideoLibrary
;; Play all of the media in the given library.
(check-expect (play-all-videos (make-mt)) (make-mt))
(check-expect (play-all-videos 
               (make-kons (make-video "Caddyshack" 1980 4)
                          (make-kons (make-video "Aladdin" 1992 40)
                                     (make-mt))))
              (make-kons (make-video "Caddyshack" 1980 5)
                         (make-kons (make-video "Aladdin" 1992 41)
                                    (make-mt))))
(define (play-all-videos l)
  (cond [(mt? l) l]
        [else (make-kons (play-video (kons-first l))
                         (play-all-videos (kons-rest l)))]))
}

@larger{@bold{Ex 4}}: This program allows users to view only new
videos in the library.

@isl-block{
;; only-new-videos : VideoLibrary -> VideoLibrary
;; View only the new items in the library.
(check-expect (only-new-videos (make-mt)) (make-mt))
(check-expect (only-new-videos
               (make-kons (make-video "Caddyshack" 1980 4)
                          (make-kons (make-video "The Replacements" 2000 0)
                                     (make-mt))))
              (make-kons (make-video "The Replacements" 2000 0)
                         (make-mt)))
(define (only-new-videos l)
  (cond [(mt? l) l]
        [else (cond [(= 0 (video-play-count (kons-first l)))
                     (make-kons (kons-first l) (only-new-videos (kons-rest l)))]
                    [else (only-new-videos (kons-rest l))])]))
}

@larger{@bold{Ex 5}}: This program extends the notion of media
to include music.

@isl-block{
;; A Music is a (make-music title album year play-count)
;; where title      is a String,
;;       album      is a String,
;;       year       is an Int,
;;   and play-count is a PositiveInt.
;; Interp: A music multimedia entry.
(define-struct music (title album year play-count))

;; A Media is one of:
;; - Video
;; - Music

;; play : Media -> Media
;; Play the given media, incrementing its play-count.
(check-expect (play (make-music "Safari Song" "From the Fires" 2017 100))
              (make-music "Safari Song" "From the Fires" 2017 101))
(check-expect (play (make-video "Caddyshack" 1980 4))
              (make-video "Caddyshack" 1980 5))
(define (play m)
  (cond [(video? m) (play-video m)]
        [else (make-music (music-title m) (music-album m)
                          (music-year m) (add1 (music-play-count m)))]))
}

@larger{@bold{Ex 6}}: Extend your data definition of @emph{Library} to
include both types of @emph{Media}. This should include
implementations of @tt{play-all} and @tt{only-new} that work on a
@emph{Library} of both @emph{Video} and @emph{Music}.
