;; This is the code for Metacircular Evaluator
;; Wei Qian (Wesley) (weschin@brandeis.edu)
;; 04-05-15


;;*****************************************************************************
;;******************************** Given CODE *********************************

;;;; METACIRCULAR EVALUATOR FROM CHAPTER 4 (SECTIONS 4.1.1-4.1.4) of
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS (SICP)

;;;; Matches code in ch4.scm, except that "eval" is "mc-eval"
;;;; and "apply" is "mc-apply".

;;;; This file can be loaded into Scheme as a whole.
;;;; Then you can initialize and start the evaluator by evaluating
;;;; the two commented-out lines at the end of the file (setting up the
;;;; global environment and starting the driver loop).

;;;from section 4.1.4 -- must precede def of metacircular apply
(define apply-in-underlying-scheme apply)

;;;SECTION 4.1.1

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (mc-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (mc-eval (first-exp exps) env))
        (else (mc-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (mc-eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (mc-eval (definition-value exp) env)
                    env)
  'ok)

;;;SECTION 4.1.2

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (variable? exp) (symbol? exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))


(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))


(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;;SECTION 4.1.3

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;;;SECTION 4.1.4

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define true #t)
(define false #f)
(define (1+ x) (+ 1 x))
(define (-1+ x) (- x 1))

; override in lab section
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'zero? zero?)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list '1+ 1+)
        (list '-1+ -1+)
        (list 'quotient quotient)
        (list 'remainder remainder)
        (list '/ /)
        (list '* *)
        (list '+ +)
        (list '- -)
        (list '= =)
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define true #t)
(define false #f)

(define the-global-environment (setup-environment))

(pp "To start the metacircular evaluator, evaluate (driver-loop)")

'METACIRCULAR-EVALUATOR-LOADED



;;***************************************************************************
;;******************************* MY CODE ***********************************

(define (mc-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env))) ; for primitive procedure, we will use the actual value
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments (cadr procedure) env) ; delay the argument acoording to the parameters set up
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (mc-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((let? exp) (eval-let exp env))
        ((let*? exp) (eval-let* exp env))
        ((unassign? exp) (eval-unassign exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((new-cons? exp) (eval-new-cons exp env)) ;update the new eval with an addiiton new-cons setup
        ((cond? exp) (mc-eval (cond->if exp) env))
        ((application? exp)
         (mc-apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

; eval-let is the let->combination 
(define (let? exp) (tagged-list? exp 'let)) ; check let statment
(define (let-variable exp) (map car (cadr exp)))  ; extract the variable
(define (let-value exp) (map cadr (cadr exp)))  ; extract the associate value
(define (let-body exp) (cddr exp))  ;use cddr because the make-lambda (cons a (list b)) will cancel a parathesis

(define (eval-let exp env) (mc-eval (cons (make-lambda (let-variable exp) (let-body exp)) (let-value exp)) env))  ;convert let to lambda

; eval-let* is the let*->nested-lets
(define (let*? exp) (tagged-list? exp 'let*)) ; check let* statement
(define (let*-init exp) (cadr exp)) ; get the let pairing
(define (let*-body exp) (caddr exp)) ; extract the lambda body

(define (eval-let* exp env)   ; convert the let* to let
  (define (let*-loop init) 
    (if (null? init)
        (let*-body exp)
        (list 'let (list (car init)) (let*-loop (cdr init)))))  ;use previous defined value to define the following variables
  (mc-eval (let*-loop (let*-init exp)) env))


; unassign varialbe
(define (unassign? exp) (tagged-list? exp 'make-unbound!))  ;check make-unbound! statement
(define (unassign-variable exp) (cadr exp)) ; extract the variable that need to be unbounded.

(define (eval-unassign exp env) ; evaluate the unassign statment
  (unbound-variable! (unassign-variable exp) env) 'ok)

(define (unbound-variable! var env) ; execute the unassign
    (define (scan vars vals frame previous-var previous-val)  ; loop through the variables in the current frame
        (define (link-previous-next previous-var previous-val next-var next-val frame)  ; a procedure linking variables
          (if (eq? (car previous-var) 'frame) ; if the unbounding variable is the first var in the frame,
            (begin (set-car! frame next-var) (set-cdr! frame next-val)) ; we reset the frame varialbe
            (begin (set-cdr! previous-var next-var)(set-cdr! previous-val next-val))))  ; otherwise we just link the previous var to the next var
      (cond ((null? vars)
              (error "Unbound Variable In Current Frame" var)) ; the unbound variable is not in the current frame
            ((eq? var (car vars))
              (link-previous-next previous-var previous-val (cdr vars) (cdr vals) frame))  ; if we get to the var, link the previous var to the next var
            (else 
              (scan (cdr vars) (cdr vals) frame (cdr previous-var) (cdr previous-val))))) ; otherwise check the remianing var list
  (let ((frame (first-frame env)))  ;only unbound the variable in the first frame of the environment
    (scan (frame-variables frame)
          (frame-values frame)
          frame
          (cons 'frame (frame-variables frame)) ; a 'frame mark for noting the unbounding variable is the first variable
          (cons 'frame (frame-values frame))))) ; a 'frame mark for noting the unbounding variable is the first variable

; get the actual value of the argument
(define (actual-value exp env)
  (force-it (mc-eval exp env)))

; return the actual value for argument
(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

; delay argument acoording to parameters set up
(define (list-of-delayed-args exps parameters env)
  (if (no-operands? exps)
      '()
      (if (and (list? (car parameters)) (eq? (car (car parameters)) 'delayed))  ; check if we need to delay
        (cons (delay-it (first-operand exps) env) ; if so, delay it
              (list-of-delayed-args (rest-operands exps) (cdr parameters) env))
        (cons (actual-value (first-operand exps) env) ; otherwise, evaluate the argument for extended enviroment binding
              (list-of-delayed-args (rest-operands exps) (cdr parameters) env))
        )))

; new driver-loop which will force out the delayed value
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;; delay function setup
(define (delay-it exp env)
  (list 'thunk exp env))
(define (thunk? obj)
  (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))
;; "thunk" that has been forced and is storing its (memoized) value
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))
(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)  ; replace exp with its value
           (set-cdr! (cdr obj) '())     ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

; hybrid prompt
(define input-prompt ";;; H-Eval input:")
(define output-prompt ";;; H-Eval value:")
  
; new way to extract parameters
(define (procedure-parameters p) 
  (define (extract-delayed obj)
    (if (and (list? obj) (eq? (car obj) 'delayed))
        (cadr obj)
        obj))
  (map extract-delayed (cadr p)))

; check if the statment is the enw cons
(define (new-cons? exp) (tagged-list? exp 'new-cons))
; setup the new cons by cons the delaying object
(define (eval-new-cons exp env) (cons (cadr exp) (delay-it (caddr exp) env)))

(driver-loop)



