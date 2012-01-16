#lang scribble/manual
@(require "../utils.rkt")

@title{Finger exercises: Designing classes}

Design classes to represent @emph{ternary} trees of numbers.  (A
ternary tree is like a binary tree, except nodes have @emph{three}
subtrees instead of two.)

In this representation, values are stored at @emph{both} nodes and
leaves, and there are no empty trees.  Note that this means that trees
with two or three values cannot be represented.

Implement the methods @tt{size} (how many numbers are there in the
tree), @tt{sum} (sums all the numbers in the tree), @tt{prod}
(computes the product of all the numbers in the tree), @tt{contains?}
(is a given number in the tree?), @tt{map} (apply a function to every
number in the tree), and @tt{max} (what's the largest number in the
tree?).

