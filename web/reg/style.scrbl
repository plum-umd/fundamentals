#lang scribble/manual
@(require "../unnumbered.rkt"
          "../utils.rkt")

@(define (ittt . args) (italic (apply tt args)))

@title*{Code style}

To follow the proper coding style for this class, you should have
Eclipse insert spaces for indentation instead of tabs. This ensures
that the code looks the same in every editor, regardless of the tab
width settings. To do so, follow these instructions:

@itemlist[#:style 'ordered
@item{In Eclipse, select Preferences in the Window menu.}
@item{In the navigation pane on the left side of the Preferences dialog, drill down to Java > Code Style > Formatter.}
@item{Click "Edit...".}
@item{Under Tab policy, select "Spaces only".}
@item{Rename the profile to something like "Mine" and click OK.}
]