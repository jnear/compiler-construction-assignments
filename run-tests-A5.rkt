#! /usr/bin/env racket
#lang racket

(require "utilities.rkt")
(require "interp-R2.rkt")
(require "compiler-A5.rkt")
(debug-level 2)

;; Define the passes to be used by interp-tests and the grader
(define r2-passes
  `( ("shrink" ,shrink ,interp-R2)
     ("uniquify" ,uniquify ,interp-R2)
     ("remove-complex-opera*" ,remove-complex-opera* ,interp-R2)
     ("explicate-control" ,explicate-control ,interp-C1)
     ("uncover-locals" ,uncover-locals ,interp-C1)
     ("select-instructions" ,select-instructions ,interp-x86)
     ("uncover-live" ,uncover-live ,interp-x86)
     ("build-interference" ,build-interference ,interp-x86)
     ("allocate-registers" ,allocate-registers ,interp-x86)
     ("patch-instructions" ,patch-instructions ,interp-x86)
     ("print-x86" ,print-x86 #f)
     ))


(interp-tests "r2" type-check-R2 r2-passes interp-R2 "r1" (tests-for "r1"))
(interp-tests "r2" type-check-R2 r2-passes interp-R2 "r2" (tests-for "r2"))
(newline)(display "Interpreter tests passed!") (newline)

(compiler-tests "r2" type-check-R2 r2-passes "r1" (tests-for "r1"))
(compiler-tests "r2" type-check-R2 r2-passes "r2" (tests-for "r2"))
(newline)(display "Compiler tests passed!") (newline)
