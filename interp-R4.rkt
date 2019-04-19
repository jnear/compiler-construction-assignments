#lang racket
(require racket/fixnum)
(require "utilities.rkt")
(require (prefix-in runtime-config: "runtime-config.rkt"))
(provide interp-R4 interp-R4-prog interp-F-top interp-C interp-pseudo-x86-top interp-x86-top)

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
    (verbose "R4/interp-exp" e)
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
      [`(has-type ,e ,t)
       (recur e)]
      [`(void) (void)]
      [`(,op ,args ...)
       #:when (set-member? primitives op)
       (apply (interp-op op) (for/list ([e args]) (recur e)))]
      [(or `(app ,fun ,args ...) `(,fun ,args ...))
       (define fun-val (recur fun))
       (define arg-vals (for/list ([e args]) (recur e)))
       (match fun-val
	 [`(lambda (,xs ...) ,body ,fun-env)
	  (define new-env (append (map cons xs arg-vals) fun-env))
	  ((interp-exp new-env) body)]
	 [else (error "interp-exp, expected function, not" fun-val)])]
      [else (error 'interp-exp "unrecognized expression" e)]
      )))

(define (interp-def d)
  (match d
    [(or `(define (,f [,xs : ,ps] ...) : ,rt ,body)
         `(define (,f [,xs : ,ps] ...) : ,rt ,_ ,body))
     (mcons f `(lambda ,xs ,body ()))]
    ))


;; Use this version after type checking
(define (interp-R4-prog p)
  (verbose "R4/interp-R4-prog" p)
  (match p
    [`(program ,info ,ds ...)
     (let ([top-level (for/list ([d ds]) (interp-def d))])
       (for ([b top-level])
         (set-mcdr! b (match (mcdr b)
                        [`(lambda ,xs ,body ())
                         `(lambda ,xs ,body ,top-level)])))
       ;; call the main function
       ((interp-exp top-level) `(main)))]
    ))

;; This is for source programs.  
(define (interp-R4 p)
  (verbose "R4/interp-R4" p)
  (match p
    [`(program ,info ,ds ... ,body)
     (let ([top-level (for/list ([d ds]) (interp-def d))])
       (for/list ([b top-level])
         (set-mcdr! b (match (mcdr b)
                        [`(lambda ,xs ,body ())
                         `(lambda ,xs ,body ,top-level)])))
       ((interp-exp top-level) body))]
    ))

;; ===========================================================

(define fst
  (lambda (p)
    (cond [(pair? p)
           (car p)]
          [(mpair? p)
           (mcar p)]
          [else
           (error 'fst "not a pair of any sort" p)])))

(define (apply-fun interp fun-val arg-vals)
  (match fun-val
    [`(lambda (,xs ...) ,body ,lam-env)
     (define new-env (append (map cons xs arg-vals) lam-env))
     ((interp new-env) body)]
    [else (error 'apply-fun "expected function, not ~a" fun-val)]))

;; ===========================================================

;; The simulated global state of the program
;; define produces private fields
(define memory (box '()))
;; field is like define but public
(define stack-size (runtime-config:rootstack-size))
(define heap-size  (runtime-config:heap-size))
(define uninitialized 'uninitialized-value-from-memory)
(define fromspace_begin (box uninitialized))
(define rootstack_end   (box uninitialized))
(define free_ptr	    (box uninitialized))
(define fromspace_end   (box uninitialized))
(define rootstack_begin (box uninitialized))
(define global-label-table
  (make-immutable-hash
   `((free_ptr	 . ,free_ptr)
     (fromspace_begin	 . ,fromspace_begin)
     (fromspace_end	 . ,fromspace_end)
     (rootstack_begin	 . ,rootstack_begin)
     (rootstack_end	 . ,rootstack_end))))

(define (allocate-page! name size)
  (verbose "allocate-page!" name size)
  (unless (and (fixnum? size)
               (positive? size)
               (= 0 (modulo size 8)))
          (error 'allocate-page! "expected non-negative fixnum in ~a" size))
  ;; Find the last address
  (define max-addr
    (for/fold ([next 8])
              ([page (in-list (unbox memory))])
              (match-let ([`(page ,_ ,stop ,_ ,_) page])
                         (max next stop))))
  ;; Allocate with a small pad 100 words so that it isn't likely to
  ;; accidentally use another region.
  ;; The randomness is to dispell any reliance on interp always allocating
  ;; the same way. -Andre
  (define start-addr (+ max-addr 800))
  ;; The range is of valid addresses in memory are [start, stop)
  (define stop-addr (+ start-addr size))
  (define vect (make-vector (arithmetic-shift size -3) uninitialized))
  (verbose "allocated" name start-addr stop-addr)
  (set-box! memory (cons `(page ,start-addr ,stop-addr ,name ,vect)
                         (unbox memory)))
  start-addr)

(define (interp-F env)
  (lambda (ast)
    (verbose "R4/interp-F" ast)
    (define result
      (match ast
        ;; For R4
        [(or `(define (,f [,xs : ,ps] ...) : ,rt ,body)
             `(define (,f [,xs : ,ps] ...) : ,rt ,_ ,body))
         (cons f `(lambda ,xs ,body))]
        [`(fun-ref ,f)
         (lookup f env)]
        [`(app ,fun ,args ...)
         (define fun-val ((interp-F env) fun))
         (define arg-vals (map (interp-F env) args))
         (match fun-val
           [`(lambda (,xs ...) ,body)
            (define new-env (append (map cons xs arg-vals) env))
            ((interp-F new-env) body)]
           [else (error "interp-F, expected function, not" fun-val)])]
        [`(program ,info ,ds ...)
         ((initialize!) runtime-config:rootstack-size
          runtime-config:heap-size)
         (let ([top-level (map  (interp-F '()) ds)])
           ((interp-F top-level) '(app main)))]
        ;; For R3
        [`(global-value free_ptr)
         (unbox free_ptr)]
        [`(global-value fromspace_end)
         (unbox fromspace_end)]
        [`(allocate ,l ,ty) (build-vector l (lambda a uninitialized))]
        [`(allocate-proxy ,ty) (build-vector 3 (lambda a uninitialized))]
        [`(collect ,size)
         (unless (exact-nonnegative-integer? size)
                 (error 'interp-F "invalid argument to collect in ~a" ast))
         (void)]
        [`(void) (void)]
        #;[`(vector-ref ,e-vec ,e-i)
        (define vec ((interp-F env) e-vec))
        (define i ((interp-F env) e-i))
        (F-vector-ref vec i)]
        #;[`(vector-set! ,e-vec ,e-i ,e-arg)
        (define vec ((interp-F env) e-vec))
        (define i ((interp-F env) e-i))
        (define arg ((interp-F env) e-arg))
        (F-vector-set! vec i arg)]
        ;; For R2
        [`(has-type ,e ,t) ((interp-F env) e)]
        [#t #t]
        [#f #f]
        [`(and ,e1 ,e2)
         (match ((interp-F env) e1)
           [#t (match ((interp-F env) e2)
                 [#t #t] [#f #f])]
           [#f #f])]
        [`(if ,cnd ,thn ,els)
         (if ((interp-F env) cnd)
             ((interp-F env) thn)
             ((interp-F env) els))]
        ;; For R1
        [(? symbol?)
         (lookup ast env)]
        [(? integer?) ast]
        [`(let ([,x ,e]) ,body)
         (let ([v ((interp-F env) e)])
           ((interp-F (cons (cons x v) env)) body))]
        [`(,op ,args ...)
         #:when (set-member? primitives op)
         (apply (interp-op op) (for/list ([e args]) ((interp-F env) e)))]
        [`(,f ,args ...)
         ((interp-F env) `(app ,f ,@args))]
        [else
         (error 'interp-F "R4 unmatched ~a" ast)]
        ))
    (verbose "R4/interp-F" ast result)
    result
    ))

(define interp-F-top
  (interp-F '()))

(define (initialize!)
  (lambda (stack-length heap_length)
    (verbose "initialize!")
    (set-box! memory '())
    (let* ([s-begin (allocate-page! 'rootstack stack-size)]
           [h-begin (allocate-page! 'fromspace heap-size)])
      (set-box! rootstack_begin s-begin)
      (set-box! rootstack_end   (+ s-begin stack-size))
      (set-box! fromspace_begin h-begin)
      (set-box! fromspace_end   (+ h-begin heap-size))
      (set-box! free_ptr	    h-begin))))

;; ==================================================

(define (interp-C-exp env)
  (lambda (ast)
    (verbose "R4/interp-C-exp" ast (map fst env))
    (match ast
      [`(fun-ref ,f)
       (lookup f env)]
      [`(call ,f ,args ...)
       (define arg-vals (map (interp-C-exp env) args))
       (define f-val ((interp-C-exp env) f))
       (match f-val
         [`(lambda (,xs ...) ,info ,CFG ,def-env)
          (define f (dict-ref info 'name))
          (define f-start (symbol-append f 'start))
          (define new-env (append (map cons xs arg-vals) def-env))
          (parameterize ([get-CFG CFG])
                        ((interp-C-tail new-env) (dict-ref CFG f-start)))]
         [else (error "interp-C, expected a function, not" f-val)])]
      [`(void) (void)]
      [`(global-value free_ptr)
       (unbox free_ptr)]
      [`(global-value fromspace_end)
       (unbox fromspace_end)]
      [`(collection-needed? ,size)
       (when (or (eq? (unbox free_ptr) uninitialized)
                 (eq? (unbox fromspace_end) uninitialized))
	     (error 'interp-C "uninitialized state in ~a" ast))
       #t]
      ;; allocate a vector of length l and type t that is initialized.
      [`(allocate ,l ,ty) (build-vector l (lambda a uninitialized))]
      [`(allocate-proxy ,ty) (build-vector 3 (lambda a uninitialized))]
      [`(has-type ,e ,t) ((interp-C-exp env) e)]
      [#t #t]
      [#f #f]
      [(? symbol? x) (lookup x env)]
      [(? integer? n) n]
      [`(,op ,args ...)
       #:when (set-member? primitives op)
       (apply (interp-op op) (map (interp-C-exp env) args))]
      )))

(define (interp-C-tail env)
  (lambda (ast)
    (verbose "R4/interp-C-tail" ast (map fst env))
    (match ast
      [`(tailcall ,f ,args ...)
       (define arg-vals (map (interp-C-exp env) args))
       (define f-val ((interp-C-exp env) f))
       (match f-val
         [`(lambda (,xs ...) ,info ,CFG ,def-env)
          (define f (dict-ref info 'name))
          (define f-start (symbol-append f 'start))
          (define new-env (append (map cons xs arg-vals) def-env))
          (parameterize ([get-CFG CFG])
                        ((interp-C-tail new-env) (dict-ref CFG f-start)))]
         [else (error "interp-C, expected a funnction, not" f-val)])]
      [`(seq ,s ,t)
       (define new-env ((interp-C-stmt env) s))
       ((interp-C-tail new-env) t)]
      [`(if ,cnd ,thn ,els)
       (if ((interp-C-exp env) cnd)
           ((interp-C-tail env) thn)
           ((interp-C-tail env) els))]
      [`(goto ,label)
       ((interp-C-tail env) (goto-label label))]
      [`(return ,e)
       ((interp-C-exp env) e)]
      )))

(define (interp-C-stmt env)
  (lambda (ast)
    (verbose "C0/interp-C-stmt" ast)
    (match ast
      [`(assign ,x ,e)
       (let ([v ((interp-C-exp env) e)])
         (cons (cons x v) env))]
      [`(collect ,size)
       (unless (exact-nonnegative-integer? ((interp-C-exp env) size))
               (error 'interp-C "invalid argument to collect in ~a" ast))
       env]
      [`(collect ,rs ,size)
       (unless (and (exact-nonnegative-integer? ((interp-C-exp env) rs))
                    (exact-nonnegative-integer? ((interp-C-exp env) size)))
               (error 'interp-C "invalid argument(s) to collect in ~a" ast))
       env]
      [else
       (error "interp-C-stmt unhandled" ast)]
      )))

(define (interp-C-def ast)
  (verbose "R4/interp-C-def" ast)
  (match ast
    [`(define (,f [,xs : ,ps] ...) : ,rt ,info ,CFG)
     (mcons f `(lambda ,xs ((name . ,f)) ,CFG ()))]
    [else
     (error "R4/interp-C-def unhandled" ast)]
    ))

(define (interp-C ast)
  (verbose "R4/interp-C" ast)
  (match ast
    [`(program ,info ,ds ...)
     ((initialize!) runtime-config:rootstack-size
      runtime-config:heap-size)
     (define top-level (for/list ([d ds]) (interp-C-def d)))
     ;; tie the knot
     (for/list ([b top-level])
               (set-mcdr! b (match (mcdr b)
                              [`(lambda ,xs ,info ,CFG ())
                               `(lambda ,xs ,info ,CFG ,top-level)])))
     ((interp-C-tail top-level) '(tailcall main))]
    [else
     (error "R4/interp-C unhandled" ast)]
    ))

(define (stack-arg-name n)
  (string->symbol (string-append "rsp_" (number->string n))))

(define (builtin-funs)
  (set 'malloc 'alloc 'collect 'initialize 'read_int))

(define byte2full-reg
  (lambda (r)
    (match r
      ['al 'rax]
      ['bl 'rbx]
      ['cl 'rcx]
      ['dl 'rdx]
      )))

(define (get-name ast)
  (match ast
    [`(stack-arg ,n) (stack-arg-name n)]
    [`(byte-reg ,r)
     (get-name `(reg ,(byte2full-reg r)))]
    [(or `(var ,x) `(reg ,x)) x]
    [`(deref rbp ,n) n]))

;; ==================================================

(define (call-function f-val cont-ss env)
  (match f-val
    [`(lambda ,info ,CFG ,def-env)
     (debug "interp-x86 call-function" f-val)
     (define f (dict-ref info 'name))
     ;; hardcoded so we don't assume anything about compiler's info field
     (define spills (cons 'unused (expt 2 13)))
     ;; copy argument registers over to new-env
     (define passing-regs
       (filter (lambda (p) p)
               (for/list ([r arg-registers])
                         (let ([v (lookup r env #f)])
                           (if v (cons r v) #f)))))
     (debug "interp-x86 call-function" passing-regs)
     (define new-env
       (cond [spills
              (define variable-size 8) ;; ugh -Jeremy
              (define root-size (* variable-size (cdr spills)))
              (cons (cons 'r15 (+ root-size (unbox rootstack_begin)))
                    (append passing-regs def-env))]
             [else
              (cons (cons 'r15 (unbox rootstack_begin)) ;; ??? -Jeremy
                    (append passing-regs def-env))]))
     (define result-env
       (parameterize ([get-CFG CFG])
                     ((interp-x86-block new-env)
                      (dict-ref CFG (symbol-append f 'start)))))
     (define res (lookup 'rax result-env))
     ((interp-x86-instr (cons (cons 'rax res) env)) cont-ss)]
    [else (error "interp-x86, expected a function, not" f-val)]))

(define (interp-x86-exp env)
  (lambda (ast)
    (vomit "R4/interp-x86-exp" ast)
    (match ast
      [`(stack-arg ,n)
       (define x (stack-arg-name n))
       (lookup x env)]
      [`(fun-ref ,f)
       (lookup f env)]

      [`(global-value ,label) (fetch-global label)]
      [`(deref ,r ,i) #:when (not (eq? r 'rbp))
       (define base ((interp-x86-exp env) `(reg ,r)))
       (define addr (+ base i))
       ((memory-read) addr)]

      [`(byte-reg ,r)
       ((interp-x86-exp env) `(reg ,(byte2full-reg r)))]
      [#t 1]
      [#f 0]
      [`(eq? ,e1 ,e2)
       (if (eq? ((interp-x86-exp env) e1)
                ((interp-x86-exp env) e2))
           1 0)]
      [`(< ,e1 ,e2)
       (if (< ((interp-x86-exp env) e1)
              ((interp-x86-exp env) e2))
           1 0)]
      [`(<= ,e1 ,e2)
       (if (<= ((interp-x86-exp env) e1)
               ((interp-x86-exp env) e2))
           1 0)]
      [`(> ,e1 ,e2)
       (if (> ((interp-x86-exp env) e1)
              ((interp-x86-exp env) e2))
           1 0)]
      [`(>= ,e1 ,e2)
       (if (>= ((interp-x86-exp env) e1)
               ((interp-x86-exp env) e2))
           1 0)]

      [(or `(var ,x) `(reg ,x))
       (lookup (get-name ast) env)]
      [`(deref ,r ,n)
       (lookup (get-name ast) env)]
      [`(int ,n) n]
      )))

(define (apply-closure clos arg cont-ss env)
  (define f ((memory-read) clos))
  (define env^ (append (list (cons 'rdi clos) (cons 'rsi arg)) env))
  (call-function f cont-ss env^))

(define (interp-x86-instr env)
  (lambda (ast)
    (when (pair? ast)
          (vomit "R4/interp-x86-instr" (car ast)))
    (match ast
      [`((leaq ,s ,d) . ,ss)
       (define x (get-name d))
       (define v ((interp-x86-exp env) s))
       ((interp-x86-instr (cons (cons x v) env)) ss)]
      [`((indirect-callq ,f) . ,ss)
       (debug "indirect callq" ast)
       (define f-val ((interp-x86-exp env) f))
       (call-function f-val ss env)]
      [`((tail-jmp ,f) . ,ss)
       (debug "tail jmp" ast)
       (define f-val ((interp-x86-exp env) f))
       (call-function f-val '() env)]
      [`((callq ,f) . ,ss) 
       #:when (not (set-member? (builtin-funs) f))
       (call-function (lookup f env) ss env)]

      [`((callq malloc) . ,ss)
       (define num-bytes ((interp-x86-exp env) '(reg rdi)))
       ((interp-x86-instr
         `((rax . ,(allocate-page! 'malloc num-bytes)) . ,env))
        ss)]
      [`((callq alloc) . ,ss)
       (define num-bytes ((interp-x86-exp env) '(reg rdi)))
       ((interp-x86-instr
         `((rax . ,(allocate-page! 'alloc num-bytes)) . ,env))
        ss)]
      [`((callq collect) . ,ss)
       (define rootstack ((interp-x86-exp env) '(reg rdi)))
       (define bytes-requested ((interp-x86-exp env) '(reg rsi)))
       ((collect!) rootstack bytes-requested)
       ((interp-x86-instr env) ss)]
      [`((movq ,s ,d) . ,ss)
       (define value   ((interp-x86-exp env) s))
       (define new-env ((interp-x86-store env) d value))
       ((interp-x86-instr new-env) ss)]
      [`((,(? x86-binary-op? binop) ,s ,d) . ,ss)
       (define src ((interp-x86-exp env) s))
       (define dst ((interp-x86-exp env) d))
       (define op  (interp-x86-op binop))
       (define new-env ((interp-x86-store env) d (op src dst)))
       ((interp-x86-instr new-env) ss)]
      [`((,(? x86-unary-op? unary-op) ,d) . ,ss)
       (define dst ((interp-x86-exp env) d))
       (define op  (interp-x86-op unary-op))
       (define new-env ((interp-x86-store env) d (op dst)))
       ((interp-x86-instr new-env) ss)]

      [`(block (lives ,lives) ,ss)
       ((interp-x86-instr env) ss)]
      [`((block (lives ,lives) ,ss))
       ((interp-x86-instr env) ss)]
      [`((set ,cc ,d) . ,ss)
       (define name (get-name d))
       (define val (eflags-status env cc))
       (verbose "set" cc val)
       ((interp-x86-instr (cons (cons name val) env)) ss)]
      ;; if's are present before patch-instructions
      [(or `((if ,cnd ,thn ,els) . ,ss)
           `((if ,cnd ,thn ,_ ,els ,_) . ,ss))
       (if (not (eq? 0 ((interp-x86-exp env) cnd)))
           ((interp-x86-instr env) (append thn ss))
           ((interp-x86-instr env) (append els ss)))]

      [`((label ,l) . ,ss)
       ((interp-x86-instr env) ss)]

      ;; cmpq performs a subq operation and examimines the state
      ;; of the result, this is done without overwriting the second
      ;; register. -andre
      ;; Notice that the syntax is very confusing
      ;; (cmpq ,s2 ,s1) (jl then) (jmp else) ...
      ;; (if (< s1 s2) then else)
      ;; The following is not maintainable -Jeremy
      #;[`((cmpq ,s2 ,s1) . ,ss)
      (let* ([v1 ((interp-x86-exp env) s1)]
      [v2 ((interp-x86-exp env) s2)]
      [v3 (- v2 v1)]
      [zero     (arithmetic-shift (b2i (eq? v3 0)) 6)]
      [sign     (arithmetic-shift (b2i (< v3 0)) 7)]
      ;; Our numbers do not overflow so this bit is always 0
      [overflow (arithmetic-shift 0 11)]
      [eflags (bitwise-ior overflow sign zero)])
      ((interp-x86-instr (cons (cons '__flag eflags) env)) ss))]
      
      [`((cmpq ,s2 ,s1) . ,ss)
       (let* ([v1 ((interp-x86-exp env) s1)]
              [v2 ((interp-x86-exp env) s2)]
              [eflags 
               (cond [(< v1 v2) 'less]
                     [(> v1 v2) 'greater]
                     [else 'equal])])
         ((interp-x86-instr (cons (cons '__flag eflags) env)) ss))]

      [`((movzbq ,s ,d) . ,ss)
       (define x (get-name d))
       (define v ((interp-x86-exp env) s))
       ((interp-x86-instr (cons (cons x v) env)) ss)]
      [`((jmp-if ,cc ,label) . ,ss)
       (cond [(eq? (eflags-status env cc) 1)
              ((interp-x86-block env) (goto-label label))]
             [else ((interp-x86-instr env) ss)])]

      ['() env]
      [`((callq read_int) . ,ss)
       ((interp-x86-instr (cons (cons 'rax (read)) env)) ss)]
      [`((movq ,s ,d) . ,ss)
       (define x (get-name d))
       (define v ((interp-x86-exp env) s))
       ((interp-x86-instr (cons (cons x v) env)) ss)]
      [`((jmp ,conclusion) . ,ss)
       #:when (string-suffix? (symbol->string conclusion) "conclusion")
       env]
      [`((jmp ,label) . ,ss)
       ((interp-x86-block env) (goto-label label))]
      [`(program ,info ,ss ...)
       (let ([env ((interp-x86-instr '()) ss)])
         (lookup 'rax env))]
      [`((,binary-op ,s ,d) . ,ss)
       (let ([s ((interp-x86-exp env) s)]
             [d ((interp-x86-exp env) d)]
             [x (get-name d)]
             [f (interp-x86-op binary-op)])
         ((interp-x86-instr (cons (cons x (f d s)) env)) ss))]
      [`((,unary-op ,d) . ,ss)
       (let ([d ((interp-x86-exp env) d)]
             [x (get-name d)]
             [f (interp-x86-op unary-op)])
         ((interp-x86-instr (cons (cons x (f d)) env)) ss))]
      )))

(define (interp-x86-def ast)
  (match ast
    [`(define (,f) ,info ,CFG)
     (mcons f `(lambda ,(dict-set info 'name f) ,CFG ()))]
    ))

;; The below applies before register allocation
(define (interp-pseudo-x86 env)
  (lambda (ast)
    (vomit "R4/interp-pseudo-x86" ast)
    (match ast
      [`(program ,info ,ds ...)
       ((initialize!) runtime-config:rootstack-size
        runtime-config:heap-size)
       (define top-level (for/list ([d ds]) (interp-x86-def d)))
       ;; tie the knot
       (for/list ([b top-level])
                 (set-mcdr! b (match (mcdr b)
                                [`(lambda ,xs ,body ())
                                 `(lambda ,xs ,body ,top-level)])))
       (define env^ (list (cons 'r15 (unbox rootstack_begin))))
       (define result-env (call-function (lookup 'main top-level) '() env^))
       (display-by-type 'Integer (lookup 'rax result-env))]
      )))

(define interp-pseudo-x86-top
  (interp-pseudo-x86 '()))

;; The below applies after register allocation -JGS
(define (interp-x86 env)
  (lambda (ast)
    (verbose "R4/interp-x86" ast)
    (match ast
      [`(program ,info ,ds ...)
       ((initialize!) runtime-config:rootstack-size
        runtime-config:heap-size)
       (define top-level (for/list ([d ds]) (interp-x86-def d)))
       ;; tie the knot
       (for/list ([b top-level])
                 (set-mcdr! b (match (mcdr b)
                                [`(lambda ,xs ,body ())
                                 `(lambda ,xs ,body ,top-level)])))
       (define env^ '())
       (define result-env
         (call-function (lookup 'main top-level) '() env^))
       (display-by-type 'Integer (lookup 'rax result-env))]
      )))

(define interp-x86-top
  (interp-x86 '()))

(define (interp-x86-op op)
  (define (err)
    (error 'interp-R1-class/interp-x86-op "unmatched ~a" op))
  (cadr (hash-ref x86-ops op err)))

(define (interp-x86-block env)
  (lambda (ast)
    (match ast
      [`(block ,info ,ss ...)
       ((interp-x86-instr env) ss)]
      [else
       (error "R1/interp-x86-block unhandled" ast)])))


(define (interp-x86-store env)
  (lambda (ast value)
    (vomit "interp-x86-store" ast value)
    (match ast
      [`(global-value ,label)
       (define loc (hash-ref global-label-table label
                             (global-value-err ast)))
       (set-box! loc value)
       env]
      [`(deref ,r ,i)
       #:when (not (eq? r 'rbp))
       (define base ((interp-x86-exp env) `(reg ,r)))
       (define addr (+ base i))
       ((memory-write!) addr value)
       env]
      [dest
       (define name (get-name dest))
       (cons (cons name value) env)])))


(define (fetch-global label)
  (let* ([err (global-value-err label)]
         [ref (hash-ref global-label-table label err)]
         [value (unbox ref)])
    (when (equal? value uninitialized)
	  (debug "fetch" global-label-table)
	  (error 'interp-R3-class/fetch-global
		 "global value, ~a, used before initialization"
		 label))
    value))

(define (fetch-page addr)
  ;; Create a string containing
  (define (fmt-err addr memory)
    (apply
     string-append
     (cons (format "address ~a out of bounds\n\tcurrent memory regions:\n"
                   addr)
           (for/list ([page (in-list (unbox memory))])
                     (match-let ([`(page ,start ,stop ,name ,_) page])
                                (format "\t\t~a\t\t[~a,~a)\n" name start stop))))))
  (unless (and (fixnum? addr)
               (positive? addr))
          (error 'fetch-page "expected non-negative fixnum in ~a" addr))
  (unless (= 0 (modulo addr 8))
          (error 'fetch-page "expected quadword alligned address in ~a" addr))
  (let search ([m (unbox memory)])
    (match m
      [`() (error 'fetch-page (fmt-err addr memory))]
      [`((page ,min ,max ,name ,vect) . ,rest-memory)
       (vomit "R3/fetch page" addr min max name vect)
       (if (and (<= min addr) (< addr max))
           (values min max name vect)
           (search rest-memory))]
      [other (error 'fetch-page "unmatched ~a" m)])))


(define (memory-read)
  (lambda (addr)
    (let-values ([(start stop name vect) (fetch-page addr)])
      (let ([value (vector-ref vect (arithmetic-shift (- addr start) -3))])
        (when (equal? value uninitialized)
	      (error 'interp-R3-class/memory-read
		     "read uninitialized memory at address ~s"
		     addr))
        value))))

(define (memory-write!)
  (lambda (addr value)
    (let-values ([(start stop name vect) (fetch-page addr)])
      (vector-set! vect (arithmetic-shift (- addr start) -3) value))))

(define (collect!)
  (lambda (rootset bytes-requested)
    (verbose "collect!" bytes-requested)
    ;; after a call to collect we must guarantee there is enough
    ;; memory to allocate the requested block of memory
    (let double-heap ([hs heap-size])
      (if (< hs bytes-requested)
          (double-heap (* 2 hs))
          (let ((h-begin (allocate-page! 'fromspace hs)))
            ;; I am only advancing the end of the heap because we
            ;; are not reclaiming memory
            (set-box! fromspace_end	  (+ h-begin hs))
            (set-box! free_ptr	  h-begin))))))

(define (eflags-status env cc)
  (define eflags ((interp-x86-exp env) '(reg __flag)))
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
         1 0)]))

(define (x86-binary-op? x)
  (let ([val (hash-ref x86-ops x #f)])
    (and val (= (car val) 2))))

(define (x86-unary-op? x)
  (let ([val (hash-ref x86-ops x #f)])
    (and val (= (car val) 1))))

(define (display-by-type ty val)
  (match ty
    ['Boolean (if val #t #f)]
    ['Integer val]
    [else (error (format "don't know how to display type ~a" ty))]))

(define x86-ops
  (make-immutable-hash
   `((addq 2 ,+)
     (imulq 2 ,*)
     (subq 2 ,(lambda (s d) (- d s)))
     (negq 1 ,-)
     (notq 1 ,bitwise-not)
     (andq 2 ,bitwise-and)
     (xorq 2 ,bitwise-xor)
     (orq 2 ,bitwise-ior)
     (salq 2 ,(lambda (n v) (arithmetic-shift v n)))
     (sarq 2 ,(lambda (n v) (arithmetic-shift v (- n)))))))

(define (global-value-err ast)
  (lambda ()
    (error 'interp-R3-class "global label is unknown in ~a" ast)))
