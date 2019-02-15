#lang racket
(require racket/fixnum)
(require "utilities.rkt")
(provide interp-R1 interp-C0 interp-x86)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.

(define (interp-exp env)
  (lambda (e)
    (match e
      [(? fixnum?) e]
      [`(read)
       (define r (read))
       (cond [(fixnum? r) r]
             [else (error 'interp-R1 "expected an integer" r)])]
      [`(- ,e)
       (define v ((interp-exp env) e))
       (fx- 0 v)]
      [`(+ ,e1 ,e2)
       (define v1 ((interp-exp env) e1))
       (define v2 ((interp-exp env) e2))
       (fx+ v1 v2)]
      [(? symbol?) (lookup e env)]
      [`(let ([,x ,e]) ,body)
       (define new-env (cons (cons x ((interp-exp env) e)) env))
       ((interp-exp new-env) body)]
      )))

(define (interp-R1 p)
  (match p
    [(or `(program ,e) `(program ,_ ,e)) ((interp-exp '()) e)]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (interp-C0-stmt env)
  (lambda (s)
    (match s
      [`(assign ,x ,e)
       (cons (cons x ((interp-exp env) e)) env)]
      )))

(define (interp-C0-tail env)
  (lambda (t)
    (match t
      [`(return ,e)
       ((interp-exp env) e)]
      [`(seq ,s ,t2)
       (define new-env ((interp-C0-stmt env) s))
       ((interp-C0-tail new-env) t2)]
      )))
  
(define (interp-C0 p)
  (match p
    [`(program ,_ ((start . ,t)))
     ((interp-C0-tail '()) t)]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (interp-x86 e)
  (define (interp* ss env)
    (match ss
     ['((retq)) (lookup '(reg rax) env)]
     [`(,instr . ,instr*) (interp* instr* (cons (interp-instr instr env) env))]))

  (define (interp-instr instr env)
    (match instr
      [`(callq read_int) `((reg rax) . ,(read))]
      [`(negq ,e) `(,e . ,(- (interp-arg e env)))]
      [`(movq ,e₁ ,e₂) `(,e₂ . ,(interp-arg e₁ env))]
      [`(addq ,e₁ ,e₂) `(,e₂ . ,(+ (interp-arg e₁ env) (interp-arg e₂ env)))]))

  (define (interp-arg arg env)
    (match arg
      [`(int ,n) n]
      [`(var ,x) (lookup `(var ,x) env)]
      [`(reg ,r) (lookup `(reg ,r) env)]
      [`(deref ,r ,o) (lookup arg env)]))

  (match e
    (`(program ,_ ((start ,ss ...))) (interp* ss '()))))
