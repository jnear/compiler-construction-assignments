#lang racket
(require "utilities.rkt")
(provide type-check)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check

(define (comparison-ops) (set 'eq? '<))

(define (primitives)
  (set-union (set '+ '- 'read)
             (comparison-ops)
             (set 'not)
             (set 'vector 'vector-ref 'vector-set!)))

(define (source-primitives)
  (set-union (primitives)
             (set '< '<= '> '>=)
             (set 'and 'or 'not)))

(define (non-apply-ast)
  (set-union (source-primitives)
             (set 'eq? 'vector 'vector-ref 'vector-set! 'if 'let 
                  'define 'program 'void 'fun-ref 'has-type
                  'collect 'allocate 'global-value)))


(define (binary-op-types)
  '((+ . ((Integer Integer) . Integer))
    (- . ((Integer Integer) . Integer))
    (and . ((Boolean Boolean) . Boolean))
    (or . ((Boolean Boolean) . Boolean))
    (< . ((Integer Integer) . Boolean))
    (<= . ((Integer Integer) . Boolean))
    (> . ((Integer Integer) . Boolean))
    (>= . ((Integer Integer) . Boolean))
    ))

(define (unary-op-types)
  '((- . ((Integer) . Integer))
    (not . ((Boolean) . Boolean))
    ))

(define (nullary-op-types)
  '((read . (() . Integer))))

(define (type-check-op op arg-types)
  (define table
    (cond
     [(eq? 2 (length arg-types)) (binary-op-types)]
     [(eq? 1 (length arg-types)) (unary-op-types)]
     [else (nullary-op-types)]))

  (match (dict-ref table op)
    [`(,param-types . ,return-type)
     (for ([at arg-types] [pt param-types]) 
          (unless (equal? at pt)
                  (error "argument type does not match parameter type"
                         (list at pt))))
     return-type]
    [else
     (error "error in table lookup for " op table)]))

(define (fun-def-name d)
  (match d
    [(or `(define (,f [,xs : ,ps] ...) : ,rt ,body)
         `(define (,f [,xs : ,ps] ...) : ,rt ,_ ,body))
     f]
    [else (error 'fun-def-name "ill-formed function definition in ~a" d)]))

(define (fun-def-type d)
  (match d
    [(or `(define (,f [,xs : ,ps] ...) : ,rt ,body)
         `(define (,f [,xs : ,ps] ...) : ,rt ,_ ,body))
     `(,@ps -> ,rt)]
    [else (error 'fun-def-type "ill-formed function definition in ~a" d)]))

(define (type-check-exp env)
  (lambda (e)
    (define recur (type-check-exp env))
    (match e
      [`(,e ,es ...)
       #:when (not (set-member? (non-apply-ast) e))
       (define-values (e^ ty) ((type-check-exp env) e))
       (define-values (e* ty*) (for/lists (e* ty*) ([e (in-list es)])
                                          ((type-check-exp env) e)))
       (match ty
         [`(,ty^* ... -> ,rt)
          (for ([arg-ty ty*] [param-ty ty^*])
               (unless (equal? arg-ty param-ty)
                       (error "argument type ~a not equal to parameter type ~a for function ~a"
                              arg-ty param-ty e)))
          (values `(has-type (app ,e^ ,@e*) ,rt) rt)]
         [else (error "expected a function, not" ty)])]

      [(? symbol? v)
       (let ([t (dict-ref env e)])
         (values `(has-type ,v ,t) t))]
      [(? integer? n) (values `(has-type ,n Integer) 'Integer)]
      [(? boolean? b) (values `(has-type ,b Boolean) 'Boolean)]
      [`(let ([,x ,e]) ,body)
       (define-values (e^ Te) (recur e))
       (define-values (b Tb) ((type-check-exp (cons `(,x . ,Te) env)) body))
       (values `(has-type (let ([,x ,e^]) ,b) ,Tb) Tb)]
      [`(if ,cnd ,thn ,els)
       (define-values (c Tc) (recur cnd))
       (define-values (t Tt) (recur thn))
       (define-values (e Te) (recur els))
       (unless (equal? Tc 'Boolean)
               (error "expected condition of if to have type Boolean, not" Tc))
       (unless (equal? Tt Te)
               (error "branches of if must have the same type, but are not"
                      (list Tt Te)))
       (values `(has-type (if ,c ,t ,e) ,Te) Te)]
      [`(eq? ,e1 ,e2)
       (define-values (e1^ T1) (recur e1))
       (define-values (e2^ T2) (recur e2))
       (unless (equal? T1 T2)
               (error "arguments of eq? must have the same type, but are not"
                      (list T1 T2)))
       (values `(has-type (eq? ,e1^ ,e2^) Boolean) 'Boolean)]
      ['(void) (values '(has-type (void) Void) 'Void)]
      [`(vector ,es ...)
       (define-values (e* t*) (for/lists (e* t*) ([e es])
                                         (recur e)))
       (let ([t `(Vector ,@t*)])
         (values `(has-type (vector ,@e*) ,t) t))]
      [`(vector-ref ,e ,i)
       (define-values (e^ t) (recur e))
       (match t
         [`(Vector ,ts ...)
          (unless (and (exact-nonnegative-integer? i) (< i (length ts)))
                  (error 'type-check-exp "invalid index ~a" i))
          (let ([t (list-ref ts i)])
            (values `(has-type (vector-ref ,e^ (has-type ,i Integer)) ,t) 
                    t))]
         [else (error "expected a vector in vector-ref, not" t)])]
      [`(vector-set! ,e ,i ,arg) 
       (define-values (e-vec t-vec) (recur e))
       (define-values (e-arg^ t-arg) (recur arg))
       (match t-vec
         [`(Vector ,ts ...)
          (unless (and (exact-nonnegative-integer? i)
                       (i . < . (length ts)))
                  (error 'type-check-exp "invalid index ~a" i))
          (unless (equal? (list-ref ts i) t-arg)
                  (error 'type-check-exp "type mismatch in vector-set! ~a ~a" 
                         (list-ref ts i) t-arg))
          (values `(has-type (vector-set! ,e-vec
                                          (has-type ,i Integer)
                                          ,e-arg^) Void) 'Void)]
         [else (error 'type-check-exp
                      "expected a vector in vector-set!, not ~a"
                      t-vec)])]
      [`(eq? ,arg1 ,arg2)
       (define-values (e1 t1) (recur arg1))
       (define-values (e2 t2) (recur arg2))
       (match* (t1 t2)
         [(`(Vector ,ts1 ...) `(Vector ,ts2 ...))
          (values `(has-type (eq? ,e1 ,e2) Boolean) 'Boolean)]
         [(other wise)
          (unless (equal? t1 t2)
                  (error "arguments of eq? must have the same type, but are not"
                         (list t1 t2)))
          (values `(has-type (eq? ,e1 ,e2) Boolean) 'Boolean)])]
      [`(,op ,es ...)
       #:when (set-member? (source-primitives) op)
       (define-values (new-es ts)
         (for/lists (new-es ts) ([e es])
                    (recur e)))
       (define t-ret (type-check-op op ts))
       (values `(has-type (,op ,@new-es) ,t-ret) t-ret)])))

(define (type-check-def env)
  (lambda (e)
    (match e
      [`(define (,f ,(and p:t* `[,xs : ,ps]) ...) : ,rt ,body)
       (define new-env (append (map cons xs ps) env))
       (define-values (body^ ty^) ((type-check-exp new-env) body))
       (unless (equal? ty^ rt)
               (error "body type ~a and return type mismatch for ~a"
                      ty^ rt e))
       `(define (,f ,@p:t*) : ,rt ,body^)]
      [else (error 'type-check-def "ill-formed function definition ~a" e)]
      )))

(define (type-check env)
  (lambda (e)
    (match e
                                        ;`(program ,info ,ds ... ,body)
      [`(program ,ds ... ,body)
       (define new-env (for/list ([d ds]) 
                                 (cons (fun-def-name d) (fun-def-type d))))
       (define ds^ (for/list ([d ds])
                             ((type-check-def new-env) d)))
       (define-values (body^ ty) ((type-check-exp new-env) body))
       (unless (equal? ty 'Integer)
               (error "result of the program must be an integer, not " ty))
       `(program () ,@ds^ ,body^)])))
