;; This is the code  Scheme-to-Scheme compiler
;; with lexical addressing, multiple arguments, letrec, call/cc, enter, exit
;; Wei Qian (Wesley) (weschin@brandeis.edu)
;; 04-05-15


;;*****************************************************************************
;;******************************** Given CODE *********************************

; override in problem 3
;(define (compile exp env-names)
;  (cond ((constant? exp)
;         (compile-constant exp))
;        ((variable? exp)
;         (compile-variable exp env-names))
;        ((letrec? exp)
;         (compile-letrec (letrec-vars exp)
;                         (letrec-vals exp)
;                         (letrec-body exp)
;                         env-names))
;        ((lambda? exp)
;         (compile-lambda (binders exp) (body exp) env-names))
;        ((if? exp)
;         (compile-if (predicate exp)
;                     (then-part exp)
;                     (else-part exp)
;                     env-names))
;        ((sequence? exp)
;         (compile-sequence (cdr exp) env-names))
;        ((enter? exp)
;         (compile-enter (enter-body exp) env-names))
;        ((exit? exp)
;         (compile-exit (exit-body exp) env-names))
;        (else ; it's an application!
;         (compile-application (function-of exp)
;                              (arguments-of exp)
;                              env-names))))

(define (compile-constant c)
  (lambda (env-values cont)
    (cont c)))

(define (compile-variable v env-names)
  (let ((a (lookup-variable v env-names)))
    (lambda (env-values cont)
      (cont (fetch env-values a)))))

(define (compile-sequence sequence env-names)
  (let ((compiled-first
        (compile (car sequence) env-names)))
    (if (null? (cdr sequence))
        compiled-first
        (let ((compiled-rest 
              (compile-sequence (cdr sequence) env-names)))
          (lambda (env-values cont)
            (compiled-first
               env-values
               (lambda (a) (compiled-rest env-values cont))))))))

(define (compile-if test then else env-names)
  (let ((test-code (compile test env-names))
        (then-code (compile then env-names))
        (else-code (compile else env-names)))
    (lambda (env-values cont)
      (test-code env-values
                 (lambda (p)
                   ((if p then-code else-code) env-values cont))))))

(define (compile-arguments args env-names)
  (if (null? args)
      (lambda (env-values cont)
         (cont '()))
      (let ((first-code (compile (car args) env-names))
            (rest-code (compile-arguments (cdr args) env-names)))
        (lambda (env-values cont)
          (first-code env-values
                      (lambda (first-value)
                        (rest-code env-values
                                   (lambda (rest-values)
                                     (cont (cons first-value 
                                                rest-values))))))))))


; override in problem 2
;(define (compile-application fun args env-names) ... )
;(define (compile-lambda binders exp env-names) ... )

(define (compile-letrec vars vals body env-names)
  (let ((new-env-names (add-frame vars env-names)))
    (let ((val-codes (map (lambda (val) (compile val new-env-names))
                          vals))
          (body-code (compile body new-env-names)))
      (lambda (env-values cont)
        (let ((new-env-values 
              (add-frame (map (lambda (v) '*UNDEFINED*) vals) env-values)))
          (bind-values val-codes (car new-env-values) new-env-values)
          (body-code new-env-values cont))))))

(define (bind-values compiled-bindings frame-values new-env-values)
  (if (null? frame-values)
      'done
      ((car compiled-bindings) 
       new-env-values
       (lambda (b)
         (set-car! frame-values b)
         (bind-values (cdr compiled-bindings)
                      (cdr frame-values)
                      new-env-values)))))

(define (compile-enter exp env-names)
  (let ((body-code (compile exp (add-frame (list '*EXIT*) env-names))))
    (lambda (env-values cont)
      (body-code (add-frame (list cont) env-values) cont))))

(define (compile-exit exp env-names)
  (let ((body-code (compile exp env-names))
        (i (lookup-variable '*EXIT* env-names)))
    (lambda (env-values cont)
      (body-code env-values (fetch env-values i)))))

(define (compile-call/cc exp env-names) ...)

; Syntax stuff

(define (begins-with atom)
  (lambda (exp)
    (if (pair? exp) (equal? (car exp) atom) #f)))

(define constant? integer?)
(define (variable? v) (not (pair? v)))

(define letrec? (begins-with 'letrec))
(define (letrec-vars exp) (map car (cadr exp)))
(define (letrec-vals exp) (map cadr (cadr exp)))
(define letrec-body caddr)

(define lambda? (begins-with 'lambda))
(define binders cadr)
(define body caddr)

(define if? (begins-with 'if))
(define predicate cadr)
(define else-part cadddr)
(define then-part caddr)

(define sequence? (begins-with 'begin))

(define enter? (begins-with 'enter))
(define exit? (begins-with 'exit))
(define enter-body cadr)
(define exit-body cadr)

(define function-of car)
(define arguments-of cdr)

; Initial environment and continuation stuff

; overide at problem 1
;(define (lookup-variable v env-names) ...)
;(define (fetch env-values a) ...)

(define (add-frame frame env)
  (cons frame env))

(define (initial-continuation v) v)

(define (extend var val bindings)
  (cons (cons var val) bindings))

(define (prim-op op)
  (lambda (x k) (k (apply op x))))

(define (1+ x) (+ 1 x))

(define initial-global-environment
  (extend 'cons (prim-op cons)
  (extend 'car (prim-op car)
  (extend 'cdr (prim-op cdr)
  (extend 'null? (prim-op null?)
  (extend 'pair? (prim-op pair?)
  (extend 'zero? (prim-op zero?)
  (extend 'true #t
  (extend 'false #f
  (extend '- (prim-op -)
  (extend  '* (prim-op *)
  (extend '+ (prim-op +)
  (extend '= (prim-op =)
  (extend '< (prim-op <)
  (extend '1+ (prim-op 1+) '() )))))))))))))))

(define initial-names (cons (map car initial-global-environment) '()))
(define initial-values (cons (map cdr initial-global-environment) '()))

(define (try exp) 
  ((compile exp initial-names) initial-values initial-continuation))

(define test1
'(letrec
   ((neg (lambda (x) (- 1 x)))
    (square (lambda (x) (* x x)))
    (fun (lambda (a b) (neg (+ (square a) (square b))))))
   (fun (fun 10 20) 30)))

(define test2
'(letrec
   ((neg (lambda (x) (- 1 x)))
    (odd (lambda (n) (if (= n 0) 0 (even (- n 1)))))
    (even (lambda (n) (if (= n 0) 1 (odd (- n 1))))))
   (even 11)))

;;***************************************************************************
;;******************************* MY CODE ***********************************

;; Problem 1

; testing
(define sample 
  '((x y z)
    (bob carol ted alice)
    (s p q r)
    (cons car cdr null? pair? zero? true false - * + = < 1+)))

(define test_1
  '(letrec ((happy (lambda (x y) (x y)))) (happy 1+ (1+ 2))))

; look up variable
(define (lookup-variable v env-names)
  (define coordinate (cons -1 -1))	; create an initial coordinate for loop. car of coordinate indicates frame and cdr of coordinate indicates the order in that frame
  (define (frames-iter v env-names)
    (define (in-frame-iter v frame)
      (cond ((null? frame) (set-cdr! coordinate -2))	; if reach the end of the frame set -2 in cdr which indicates there is no value in the frame
            ((eq? v (car frame)) (set-cdr! coordinate (+ 1 (cdr coordinate))))	;if there is the request variable, increment the cdr of coordinate and stop
            (else (begin (set-cdr! coordinate (+ 1 (cdr coordinate))) (in-frame-iter v (cdr frame))))))	;continue incrementing the variable in frame
    (if (null? env-names)	; if reach to the end of the whole environment, throw error.
        (error "Unbound variable")
        (begin 	(set-car! coordinate (+ 1 (car coordinate)))	;else increment the counting of frame
          		(set-cdr! coordinate -1)	; and reset the counting for in-frame variable
            	(in-frame-iter v (car env-names))
             	(if (= (cdr coordinate) -2); if the require varialbe is not in the frame
                  	(frames-iter v (cdr env-names))	; go to the next frame
                   	(list (car coordinate) (cdr coordinate))))))	;if the require varialbe is in this frame, make the coordinate into a list and return this list.
  (frames-iter v env-names))

; fetching variable by coordinate
(define (fetch env-values a)
  (define coordinate (cons (car a) (cadr a)))	; convert the coordinate list to a coordinate cons cell
  (define (get-to-frame env-values)	; a function count down the car of coordinate and return the frame
    (if (= 0 (car coordinate))
        (car env-values)
        (begin (set-car! coordinate (- (car coordinate) 1)) (get-to-frame (cdr env-values)))))
  (define (get-to-variable frame)	; a function count down the cdr of the coordinate and return the varialbe
    (if (= 0 (cdr coordinate))
        (car frame)
        (begin (set-cdr! coordinate (- (cdr coordinate) 1)) (get-to-variable (cdr frame)))))
  (get-to-variable (get-to-frame env-values)))	; first get the frame, and then search in that frame


;; Problem 2

(define (compile-application fun arg env-names)
  (let ((fun-code (compile fun env-names))
        (arg-code (compile-arguments arg env-names)))	; instead of compile only one argument, use compile-arguments create a list of compile argument with the same continuation
    (lambda (env-values cont)
      (fun-code env-values
                (lambda (f)
                  (arg-code env-values
                            (lambda (a)	; after we generate the list of argument, apply it to f
                              (f a cont))))))))

; the compile lambda provided in the handout
(define (compile-lambda v exp env-names)
  (let ((body-code (compile exp (cons v env-names))))	; compile the body with the argument v
    (lambda (env-values cont)
      (cont (lambda (x k)
			(body-code (cons x env-values) k))))))


;; Problem 3

;testing
(define test_2 '(call/cc (lambda (k) (+ 5 (k (+ 1 3))))))

(define test_3 '((lambda (x y) (* (+ x y) (call/cc (lambda (c) (- x (c y)))))) 10 2))

; new compile with call/cc recognition
(define (compile exp env-names)
  (cond ((constant? exp)
         (compile-constant exp))
        ((variable? exp)
         (compile-variable exp env-names))
        ((letrec? exp)
         (compile-letrec (letrec-vars exp)
                         (letrec-vals exp)
                         (letrec-body exp)
                         env-names))
        ((lambda? exp)
         (compile-lambda (binders exp) (body exp) env-names))
        ((call/cc? exp)
         (compile-call/cc exp env-names))	; add new compile call/cc
        ((if? exp)
         (compile-if (predicate exp)
                     (then-part exp)
                     (else-part exp)
                     env-names))
        ((sequence? exp)
         (compile-sequence (cdr exp) env-names))
        ((enter? exp)
         (compile-enter (enter-body exp) env-names))
        ((exit? exp)
         (compile-exit (exit-body exp) env-names))
        (else ; it's an application!
         (compile-application (function-of exp)
                              (arguments-of exp)
                              env-names))))

; check tag
(define call/cc? (begins-with 'call/cc))

; compile call/cc
(define (compile-call/cc exp env-names)
  (let* ((lambda-exp (cadr exp))	; extract the lambda function
        (fun-code (compile-lambda (binders lambda-exp) (body lambda-exp) env-names)))	; compile the lambda function
    (lambda (env-values cont)
      (fun-code env-values 	;apply the lambda function where the varialbe is set to (lambda (x k) (apply cont x))
                (lambda (f)	;(lambda (x k) (apply cont x)) will ignore the most current continuation k and execute the continuation with the current continuation
                  (f (cons (lambda (x k) (apply cont x)) env-values) cont))))))


;; Problem 4

; testing
(define ff
  (try '(letrec ((fact
                   (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))))
          fact)))
(define ee
  (try '(letrec ((expt
                   (lambda (b e) (if (= e 0) 1 (* b (expt b (- e 1)))))))
          expt)))

; linking procedure
(define (link f)
  (lambda w (f w initial-continuation)))	; take a list of argument w and apply to f with an initial-continuation (i.e. identity function)

