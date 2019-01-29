#lang racket
(require racket/set racket/stream)
(require racket/fixnum)
(require racket/trace)
(require "interp-R1.rkt")
(require "utilities.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assignment 2 Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; produces slightly nicer variable names than the built-in gensym
(define gensym
  (let ([counter 1])
    (lambda (x)
      (begin0 (string->unreadable-symbol
               (format "~a.~a" x counter))
              (set! counter (add1 counter))))))

(define (your-code-here)
  (error "insert your code here!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniquify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; uniquify : env -> R1 -> R1
(define (uniquify e)
  ;; uniquify-exp : R1 -> R1
  (define (uniquify-exp e env)
    (match e
      [(? symbol?) (your-code-here)]
      [(? integer?) e]
      [`(let ([,x ,e]) ,body) (your-code-here)]
      [`(,op ,es ...) (your-code-here)]))

  (match e
    [`(program ,info ,e) `(program ,info ,(uniquify-exp e '()))]
    [`(program ,e) `(program () ,(uniquify-exp e '()))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; remove-complex-opera*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove-complex-opera* : R1 -> R1
(define (remove-complex-opera* e)
  ;; rco-exp : R1 -> R1
  (define (rco-exp e)
    (match e
      [(? symbol?) (your-code-here)]
      [(? integer?) (your-code-here)]
      [`(let ([,x ,e]) ,body) (your-code-here)]
      [`(,op ,es ...) (your-code-here)]))

  ;; note: it may be helpful to define more helpers here

  ;; rco-arg : R1 → (var, [(var, R1)])
  (define (rco-arg e)
    (match e
      [(? symbol?) `(,e . ())] ;; student code
      [(? integer?) `(,e . ())] 
      [`(let ([,x ,e]) ,body) (your-code-here)]
      [`(,op ,es ...) (your-code-here)]))

  (match e
    [`(program ,info ,e) `(program ,info ,(rco-exp e))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; explicate-control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; explicate-control : R1 → C0
(define (explicate-control e)
  ;; explicate-control-tail : R1 → C0 tail
  (define (explicate-control-tail e)
    (match e
      [(? symbol?) (your-code-here)]
      [(? integer?) `(return ,e)]
      [`(let ([,x ,e]) ,body) (your-code-here)]
      [`(,op ,es ...) (your-code-here)]))

  ;; explicate-control-assign : R1 → var → C0 tail → C0 tail
  (define (explicate-control-assign e x k)
    (match e
      [(? symbol?) (your-code-here)]
      [(? integer?) `(seq (assign ,x ,e) ,k)]
      [`(let ([,x2 ,e]) ,body) (your-code-here)]
      [`(,op ,es ...) (your-code-here)]))

  (match e
    [`(program ,info ,e) `(program ,info ((start . ,(explicate-control-tail e))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uncover-locals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; uncover-locals : C0 → C0
(define (uncover-locals e)
  ;; uncover-locals-labels : [(label . C0)] → [var]
  (define (uncover-locals-labels ls)
    (append* (map uncover-locals-tail (map cdr ls))))

  ;; uncover-locals-stmt : C0 stmt → [var]
  (define (uncover-locals-stmt e)
    (match e
      [`(assign ,x ,e) (your-code-here)]))

  ;; uncover-locals-tail : C0 tail → [var]
  (define (uncover-locals-tail e)
    (match e
      [`(return ,e) (your-code-here)]
      [`(seq ,s ,t2) (your-code-here)]))

  (match e
    [`(program ,info ,e) `(program ((locals . ,(uncover-locals-labels e)) . ,info)
                                   ,e)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; select-instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; select-instructions : C0 -> pseudo-x86
(define (select-instructions e)
  ;; C0 arg → x86 arg
  (define (si-arg e)
    (match e
      [(? fixnum?) `(int ,e)]
      [(? symbol?) `(var ,e)]
      [`(reg ,r) `(reg ,r)]))

  ;; C0 stmt → [instr]
  (define (si-stmt e)
    (match e
      [`(assign ,x (read)) (your-code-here)]
      [`(assign ,x (- ,e)) (your-code-here)]
      [`(assign ,x (+ ,e1 ,e2)) (your-code-here)]
      [`(assign ,x ,a) (your-code-here)]))

  ;; C0 tail → [instr]
  (define (si-tail e)
    (match e
      [`(return ,e) (your-code-here)]
      [`(seq ,s ,t2) (your-code-here)]))
  
  (match e
    [`(program ,info ((start . ,t))) 
     `(program ,info ((start . ,(si-tail t))))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assign-homes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; assign-homes : homes -> pseudo-x86 -> pseudo-x86
(define (assign-homes e)
  (define all-registers '(r8 r9 r10 r11 r12 r13 r14 r15))

  (define (as* ss homes)
    (map (as-instr homes) ss))

  (define ((as-instr homes) instr)
    (match instr
      [`(retq) `(retq)]
      [`(negq ,e) (your-code-here)]
      [`(movq ,e1 ,e2) (your-code-here)]
      [`(addq ,e1 ,e2) (your-code-here)]))

  (define (as-arg arg homes)
    (match arg
      [`(int ,n) (your-code-here)]
      [`(var ,x) (your-code-here)]
      [`(reg ,r) (your-code-here)]))

  (match e
    (`(program ,info ((start ,ss ...))) 
     ;; a trivial assignment from locals to registers
     (let* ((locals (lookup 'locals info))
            (regs (take all-registers (length locals)))
            (homes (map list locals regs)))
       `(program ,info ((start ,@(as* ss homes))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; print-x86
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; print-x86 : x86 -> string

;; EXAMPLE: 
;; 	.globl _main
;; _main:
;; 	movq	$10, %rax
;; 	addq	$32, %rax
;;      movq    %rax, %rdi
;; 	retq

(define (print-x86 e)
  (define (p-x86* ss)
    (string-join (map p-x86-instr ss) "\n"))

  (define (p-x86-instr instr)
    (match instr
      [`(retq) (your-code-here)]
      [`(negq ,e) (your-code-here)]
      [`(movq ,e1 ,e2) (your-code-here)]
      [`(addq ,e1 ,e2) (your-code-here)]))

  (define (p-x86-arg arg)
    (match arg
      [`(int ,n) (string-append "$" (number->string n))]
      [`(reg ,r) (string-append "%" (symbol->string r))]))

  (match e
    (`(program ,info ((start ,ss ...))) 
     (let ((instructions (p-x86* ss)))
       (your-code-here)))))
