#lang racket
(require racket/fixnum)
(require racket/trace)
(require "utilities.rkt")
(require (prefix-in runtime-config: "runtime-config.rkt"))
(provide interp-R3 interp-C2 interp-x86)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.

(define primitives (set '+ '- 'read
                        'eq? '< '<= '> '>= 'not 
                        'vector 'vector-ref 'vector-set!))

(define (interp-op op)
  (match op
    ['+ fx+]
    ['- fx-]
    ['read read-fixnum]
    ['not (lambda (v) (match v [#t #f] [#f #t]))]
    ['eq? (lambda (v1 v2)
	    (cond [(or (and (fixnum? v1) (fixnum? v2))
		       (and (boolean? v1) (boolean? v2))
		       (and (vector? v1) (vector? v2))
                       (and (void? v1) (void? v2)))
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
    ['vector vector]
    ['vector-ref vector-ref]
    ['vector-set! vector-set!]
    [else (error 'interp-op "unknown operator")]
    ))

(define (interp-exp env)
  (lambda (e)
    (define recur (interp-exp env))
    (match e
      [(? symbol?) (lookup e env)]
      [`(has-type ,e ,t)
       (recur e)]
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
      [`(global-value free_ptr) 0]
      [`(global-value fromspace_end) 1000000]
      [`(void) (void)]
      [`(allocate ,s (Vector ,ts ...)) (make-vector s)]
      [`(,op ,args ...)
       #:when (set-member? primitives op)
       (apply (interp-op op) (for/list ([e args]) (recur e)))]
      [else
       (error 'interp-exp "R3: unmatch ~a" e)]
      )))

(define (interp-R3 p)
  (match p
    [`(program ,info ,e)
     ((interp-exp '()) e)]
    [`(program ,e)
     ((interp-exp '()) e)]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (interp-C2-stmt env)
  (lambda (s)
    (match s
      [`(assign ,x ,e)
       (cons (cons x ((interp-exp env) e)) env)]
      [else
       (error "interp-C2-stmt unmatched" s)]
      )))

(define (interp-C2-tail env CFG)
  (lambda (t)
    (match t
      [`(return ,e)
       ((interp-exp env) e)]
      [`(goto ,l)
       ((interp-C2-tail env CFG) (dict-ref CFG l))]
      [`(if (,op ,arg* ...) (goto ,thn-label) (goto ,els-label))
       (if ((interp-exp env) `(,op ,@arg*))
           ((interp-C2-tail env CFG) (dict-ref CFG thn-label))
           ((interp-C2-tail env CFG) (dict-ref CFG els-label)))]
      [`(seq ,s ,t2)
       (define new-env ((interp-C2-stmt env) s))
       ((interp-C2-tail new-env CFG) t2)]
      [else
       (error "interp-C2-tail unmatched" t)]
      )))
  
(define (interp-C2 p)
  (match p
    [`(program ,info ,CFG)
     ((interp-C2-tail '() CFG) (dict-ref CFG 'start))]
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (interp-x86 e)
  (define memory          '())
  (define stack-size      (runtime-config:rootstack-size))
  (define heap-size       (runtime-config:heap-size))
  (define uninitialized   'uninitialized-value-from-memory)
  (define fromspace_begin (box uninitialized))
  (define rootstack_end   (box uninitialized))
  (define free_ptr	  (box uninitialized))
  (define fromspace_end   (box uninitialized))
  (define rootstack_begin (box uninitialized))

  (define (memory-set! addr val)
    (set! memory (cons `(,addr . ,val) memory)))

  (define (memory-get addr)
    (cond
     [(assq addr memory) => cdr]
     [else uninitialized]))

  (define (initialize! stack-length heap_length)
    (set! memory '())
    (let* ([s-begin 0]
           [h-begin (+ stack-length 8)])
      (set-box! rootstack_begin s-begin)
      (set-box! rootstack_end   (+ s-begin stack-size))
      (set-box! fromspace_begin h-begin)
      (set-box! fromspace_end   (+ h-begin heap-size))
      (set-box! free_ptr	h-begin)))

  (define (collect! rootstack bytes-requested)
    (let ([difference (- (+ (unbox free_ptr) bytes-requested) (unbox fromspace_end))])
      (when (<= difference 0)
            (error 'collect! "No collection was needed"))
      (set-box! fromspace_end (+ (unbox fromspace_end) difference))
    ))
  
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
      [`(var ,v) `(var ,v)]
      [`(deref ,r ,o) ast]))

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

  (define (interp-store dest env val)
    (match dest
      [`(global-value rootstack_begin) 
       (set-box! rootstack_begin val)
       env]
      [`(global-value rootstack_end)
       (set-box! rootstack_end val)
       env]
      [`(global-value fromspace_begin) 
       (set-box! fromspace_begin val)
       env]
      [`(global-value fromspace_end) 
       (set-box! fromspace_end val)
       env]
      [`(global-value free_ptr) 
       (set-box! free_ptr val)
       env]

      [`(global-value ,v) (error "unknown global value: ~a" v)]

      [`(deref ,r ,i) #:when (not (eq? r 'rbp))
       (let* ([base (interp-arg `(reg ,r) env)]
              [addr (+ base i)])
         (memory-set! addr val)
         env)]
      [dest
       (let ([name (get-name dest)])
         (cons `(,name . ,val) env))]))

  (define (interp-instr instrs cfg env)
    (match instrs
      ['((retq))
       ;(printf "Memory contents: ~a\n" memory)
       (lookup '(reg rax) env)]
      [`((xorq (int 1) ,e) . ,ss)
       (interp-instr ss cfg (cons `(,e . ,(if (eq? (interp-arg e env) 1) 0 1)) env))]
      [`((callq initialize) . ,ss)
       (initialize! (runtime-config:rootstack-size)
                    (runtime-config:heap-size))
       (interp-instr ss cfg env)]
      [`((callq collect) . ,ss)
       (let ([rootstack (interp-arg '(reg rdi) env)]
             [bytes-requested (interp-arg '(reg rsi) env)])
         (collect! rootstack bytes-requested)
         (interp-instr ss cfg env))]
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
       (let* ([v (interp-arg e env)]
              [new-env (interp-store e env (- v))])
         (interp-instr ss cfg new-env))]
      [`((movq ,e₁ ,e₂) . ,ss)
       (let ([new-env (interp-store e₂ env (interp-arg e₁ env))])
         (interp-instr ss cfg new-env))]
      [`((addq ,e₁ ,e₂) . ,ss)
       (let* ([v₁ (interp-arg e₁ env)]
              [v₂ (interp-arg e₂ env)]
              [new-env (interp-store e₂ env (+ v₁ v₂))])
         (interp-instr ss cfg new-env))]))

  (define (interp-arg arg env)
    (match arg
      [`(int ,n) n]
      [`(var ,x) (lookup `(var ,x) env)]
      [`(reg ,r) (lookup `(reg ,r) env)]
      [`(deref rbp ,o) (lookup arg env)]
      [`(deref ,r ,o)
       (let ([result (memory-get (+ (lookup `(reg ,r) env) o))])
         (when (eq? result uninitialized)
               (printf "Warning: dereferenced uninitialized memory!\n"))
         result)]
      [`(global-value free_ptr) (unbox free_ptr)]
      [`(global-value fromspace_end) (unbox fromspace_end)]))

  (match e
    [`(program ,_ ,blocks)
     (initialize! (runtime-config:rootstack-size)
                  (runtime-config:heap-size))
     (let ([env `(((reg r15) . ,(unbox rootstack_begin)))])
       (interp-block 'start blocks env))]))
