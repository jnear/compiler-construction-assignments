#lang racket
(require racket/set racket/stream)
(require racket/fixnum)
(require racket/trace)
(require racket/set)
(require graph)
(require "utilities.rkt")
(provide (all-defined-out))

(define caller-saved-registers '(rdx rcx rsi rdi r8 r9 r10 r11))
(define callee-saved-registers '(rbx r12 r13 r14 r15))
(define all-regs (map (λ (r) `(reg ,r)) (append caller-saved-registers callee-saved-registers)))
(define comparison-ops '(eq? <))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assignment 5 Typechecker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (type-check-exp e env)
  (match e
    [(? fixnum?)   'Integer]
    [(? boolean?)  'Boolean]
    [(? symbol? x) (dict-ref env x)]
    [`(read)       'Integer]
    [`(let ([,x ,e]) ,body)
     (let* ([te (type-check-exp e env)]
            [new-env (cons `(,x . ,te) env)])
       (type-check-exp body new-env))]
;    ...
    [`(not ,e)
     (match (type-check-exp e env)
       ['Boolean 'Boolean]
       [else (error 'type-check-exp "'not' expects a Boolean" e)])]
;    ...
    ))

(define (type-check-R2 e)
  (match e
    [`(program ,body)
     (type-check-exp body '())
     `(program ,body)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assignment 5 Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; produces slightly nicer variable names than the built-in gensym
(define gensym
  (let ([counter 1])
    (lambda (x)
      (begin0 (string->unreadable-symbol
               (format "~a.~a" x counter))
              (set! counter (add1 counter))))))


;; INSERT YOUR PASSES HERE
