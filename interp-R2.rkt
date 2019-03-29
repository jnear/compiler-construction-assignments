#lang racket
(require racket/fixnum)
(require "utilities.rkt")
(provide interp-R2 interp-C1 interp-x86)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.

(define primitives (set '+ '-  'read
                        'eq? '< '<= '> '>= 'not))

(define (interp-op op)
  (match op
    ['+ fx+]
    ['- fx-]
    ['read read-fixnum]
    ['not (lambda (v) (match v [#t #f] [#f #t]))]
    ['eq? (lambda (v1 v2)
	    (cond [(or (and (fixnum? v1) (fixnum? v2))
		       (and (boolean? v1) (boolean? v2))
		       (and (vector? v1) (vector? v2)))
		   (eq? v1 v2)]))]
    ['< (lambda (v1 v2)
	  (cond [(and (fixnum? v1) (fixnum? v2))
		 (< v1 v2)]))]
    ['<= (lambda (v1 v2)
	   (cond [(and (fixnum? v1) (fixnum? v2))
		  (<= v1 v2)]))]
    ['> (lambda (v1 v2)
	  (cond [(and (fixnum? v1) (fixnum? v2))
		 (> v1 v2)]))]
    ['>= (lambda (v1 v2)
	   (cond [(and (fixnum? v1) (fixnum? v2))
		  (>= v1 v2)]))]
    [else (error 'interp-op "unknown operator")]
    ))

(define (interp-exp env)
  (lambda (e)
    (define recur (interp-exp env))
    (match e
      [(? symbol?) (lookup e env)]
      [`(let ([,x ,e]) ,body)
       (define new-env (cons (cons x ((interp-exp env) e)) env))
       ((interp-exp new-env) body)]
      [(? fixnum?) e]
      [(? boolean?) e]
      [`(if ,cnd ,thn ,els)
       (define b (recur cnd))
       (match b
         [#t (recur thn)]
         [#f (recur els)])]
      [`(and ,e1 ,e2)
       (define v1 (recur e1))
       (match v1
         [#t (match (recur e2) [#t #t] [#f #f])]
         [#f #f])]
      [`(or ,e1 ,e2)
       (define v1 (recur e1))
       (match v1
         [#t #t]
         [#f (match (recur e2) [#t #t] [#f #f])])]
      [`(,op ,args ...)
       #:when (set-member? primitives op)
       (apply (interp-op op) (for/list ([e args]) (recur e)))]
      [else
       (error 'interp-exp "R2: unmatch" e)]
      )))

(define (interp-R2 p)
  (match p
    [`(program ,info ,e)
     ((interp-exp '()) e)]
    [`(program ,e)
     ((interp-exp '()) e)]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (interp-C1-stmt env)
  (lambda (s)
    (match s
      [`(assign ,x ,e)
       (cons (cons x ((interp-exp env) e)) env)]
      [else
       (error "interp-C1-stmt unmatched" s)]
      )))

(define (interp-C1-tail env CFG)
  (lambda (t)
    (match t
      [`(return ,e)
       ((interp-exp env) e)]
      [`(goto ,l)
       ((interp-C1-tail env CFG) (dict-ref CFG l))]
      [`(if (,op ,arg* ...) (goto ,thn-label) (goto ,els-label))
       (if ((interp-exp env) `(,op ,@arg*))
           ((interp-C1-tail env CFG) (dict-ref CFG thn-label))
           ((interp-C1-tail env CFG) (dict-ref CFG els-label)))]
      [`(seq ,s ,t2)
       (define new-env ((interp-C1-stmt env) s))
       ((interp-C1-tail new-env CFG) t2)]
      [else
       (error "interp-C1-tail unmatched" t)]
      )))
  
(define (interp-C1 p)
  (match p
    [`(program ,info ,CFG)
     ((interp-C1-tail '() CFG) (dict-ref CFG 'start))]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (interp-x86 e)

  (define byte2full-reg
    (lambda (r)
      (match r
	['al 'rax]
	['bl 'rbx]
	['cl 'rcx]
	['dl 'rdx])))

  (define (get-name ast)
    (match ast
      [`(byte-reg ,r)
       `(reg ,(byte2full-reg r))]
      [`(reg ,r) `(reg ,r)]
      [`(var ,v) `(var ,v)]))

  (define (eflags-status env cc)
    (let ([eflags (lookup '__flag env)])
      (match cc
        ['e (if (equal? eflags 'equal) 1 0)]
        ['l (if (equal? eflags 'less) 1 0)]
        ['le
         (if (or (eq? 1 (eflags-status env 'e))
	         (eq? 1 (eflags-status env 'l)))
	     1 0)]
        ['g
         (if (not (eq? 1 (eflags-status env 'le)))
	     1 0)]
        ['ge
         (if (not (eq? 1 (eflags-status env 'l)))
	     1 0)])))

  (define (interp-block name cfg env)
    (match (lookup name cfg)
      [`(block ,info ,ss)
       (interp-instr ss cfg env)]
      [`,ss (interp-instr ss cfg env)]))

  (define (interp-instr instrs cfg env)
    (match instrs
      ['((retq)) 
       (lookup '(reg rax) env)]
      [`((xorq (int 1) ,e) . ,ss)
       (interp-instr ss cfg (cons `(,e . ,(if (eq? (interp-arg e env) 1) 0 1)) env))]
      [`((callq read_int) . ,ss) 
       (interp-instr ss cfg (cons `((reg rax) . ,(read)) env))]
      [`((set ,cc ,d) . ,ss)
       (let ([name (get-name d)]
             [val (eflags-status env cc)])
         (interp-instr ss cfg (cons `(,name . ,val) env)))]
      [`((movzbq ,s ,d) . ,ss)
       (let* ([x (get-name s)]
              [val (interp-arg x env)])
         (interp-instr ss cfg (cons `(,d . ,val) env)))]
      [`((cmpq ,s2 ,s1) . ,ss)
       (let* ([v1 (interp-arg s1 env)]
              [v2 (interp-arg s2 env)]
              [eflags 
               (cond [(< v1 v2) 'less]
                     [(> v1 v2) 'greater]
                     [else      'equal])])
         (interp-instr ss cfg (cons (cons '__flag eflags) env)))]
      [`((jmp-if e ,l) . ,ss)
       (if (eq? (lookup '__flag env) 'equal)
           (interp-block l cfg env)
           (interp-instr ss cfg env))]
      [`((jmp-if l ,l) . ,ss)
       (if (eq? (lookup '__flag env) 'less)
           (interp-block l cfg env)
           (interp-instr ss cfg env))]
      [`((jmp ,l) . ,ss)
       (interp-block l cfg env)]
      [`((negq ,e) . ,ss) 
       (interp-instr ss cfg (cons `(,e . ,(- (interp-arg e env))) env))]
      [`((movq ,e₁ ,e₂) . ,ss) 
       (interp-instr ss cfg (cons `(,e₂ . ,(interp-arg e₁ env)) env))]
      [`((addq ,e₁ ,e₂) . ,ss) 
       (interp-instr ss cfg (cons `(,e₂ . ,(+ (interp-arg e₁ env) (interp-arg e₂ env))) env))]))

  (define (interp-arg arg env)
    (match arg
      [`(int ,n) n]
      [`(var ,x) (lookup `(var ,x) env)]
      [`(reg ,r) (lookup `(reg ,r) env)]
      [`(deref ,r ,o) (lookup arg env)]))

  (match e
    [`(program ,_ ,blocks) 
     (interp-block 'start blocks '())]))
