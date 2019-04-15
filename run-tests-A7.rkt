#! /usr/bin/env racket
#lang racket

(require "utilities.rkt")
(require "interp-R4.rkt")
(require "compiler-A7.rkt")
(debug-level 2)

;; Define the passes to be used by interp-tests and the grader

(define r4-passes
  `(
    ("shrink" ,shrink
     ,interp-R4-prog)
    ("uniquify"  ,uniquify
     ,interp-R4-prog)
    ("reveal-functions" ,reveal-functions
     ,interp-F-top)
    ("limit-functions" ,limit-functions
     ,interp-F-top)
    ("expose allocation" ,expose-allocation
     ,interp-F-top)
    ("remove-complex-opera*" ,remove-complex-opera*
     ,interp-F-top)
    ("explicate-control" ,explicate-control
     ,interp-C)
    ("optimize-jumps" ,optimize-jumps
     ,interp-C)
    ("uncover-locals" ,uncover-locals
     ,interp-C)
    ("instruction selection" ,select-instructions
     ,interp-pseudo-x86-top)
    ("remove-jumps" ,remove-jumps
     ,interp-pseudo-x86-top)
    ("uncover-live" ,uncover-live
     ,interp-pseudo-x86-top)
    ("build-interference" ,build-interference
     ,interp-pseudo-x86-top)
    ("build-move-graph" ,build-move-graph
     ,interp-pseudo-x86-top)
    ("allocate-registers" ,allocate-registers
     ,interp-x86-top)
    ("patch instructions" ,patch-instructions
     ,interp-x86-top)
    ("print x86" ,print-x86 #f)
    ))



(interp-tests "r4" type-check-R4 r4-passes interp-R4 "r4" (tests-for "r4"))
(newline)(display "Interpreter tests passed!") (newline)

(compiler-tests "r4" type-check-R4 r4-passes "r4" (tests-for "r4"))
(newline)(display "Compiler tests passed!") (newline)

