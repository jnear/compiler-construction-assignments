#! /usr/bin/env racket
#lang racket

(require "utilities.rkt")
(require "interp-R1.rkt")
(require "compiler-A2.rkt")
(debug-level 1)

;; Define the passes to be used by interp-tests and the grader
;; Note that your compiler file (or whatever file provides your passes)
;; should be named "compiler.rkt"
(define r1-passes
  `( ("uniquify" ,(uniquify '()) ,interp-R1)
     ("remove-complex-opera*" ,remove-complex-opera* ,interp-R1)
     ("explicate-control" ,explicate-control ,interp-C0)
     ("uncover-locals" ,uncover-locals ,interp-C0)
     ("select-instructions" ,select-instructions ,interp-x86)
     ("assign-homes" ,assign-homes ,interp-x86)
     ("print-x86" ,print-x86 #f)
     ))

(interp-tests "r1" #f r1-passes interp-R1 "r1" (tests-for "r1"))
(newline)(display "Interpreter tests passed!") (newline)

(compiler-tests "r1" #f r1-passes "r1" (tests-for "r1"))
(newline)(display "Compiler tests passed!") (newline)
