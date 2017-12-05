;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some functions for manipulating lambda expressions.
;;
;; (make-abs <var> <expr>) creates the encoding for an abstraction
;; (var-of <abs>) return the variable of an abstraction
;; (body-of <abs>) return the body of an abstraction
;;
;; (make-app <rator> <rand>) creates the encoding for an application
;; (rator-of <app>) return the function of an application
;; (rand-of <app>) return the argument of an application
;;
;; (abs? <expr>) predicate to identify abstractions
;; (app? <expr>) predicate to identify applications
;; (var? <expr>) predicate to identify variables
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-abs (lambda (var body) (list 'fun var body)))
(define make-app (lambda (rator rand) (list rator rand)))
(define abs? (lambda (expr) 
  (and (list? expr) (= (length expr) 3) (eqv? 'fun (car expr)))))
(define app? (lambda (expr)
  (and (list? expr) (= (length expr) 2))))
(define var? symbol?)
(define var-of cadr)
(define body-of caddr)
(define rator-of car)
(define rand-of cadr)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Static-typed subst
;;
;; I have chosen my username and a suffix at the end of it (which will be incremented as new names are assigned)
;; to denote unique class of new names in order to satisfy the correct definition of substitution.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define suf 0)
(define subst (lambda (e x expr)
                 (letrec ((newVar (lambda (name) (set! suf (+ suf 1)) (string->symbol (string-append "usharma" (number->string suf)))))
                          (free? (lambda (x expr)
                                        (cond
                                          ((var? expr) (eqv? expr x))
                                          ((abs? expr) (if (eqv? (var-of expr) x) #f (free? x (body-of expr))))
                                          ((app? expr) (or (free? (rator-of expr) x) (free? (rand-of expr) x)))))))
    (cond 
      ((abs? expr)
         (if (eqv? x (var-of expr))
             expr  ;; [e/x](fun x e1) = (fun x e1)
             (if (free? (var-of expr) e) (let ((new (newVar (var-of expr)))) (make-abs new (subst e x (subst new (var-of expr) (body-of expr))))) ;; [e/x] (fun y e1) = (fun [m/y]y [e/x]e1) (y!=x, y in FV(e1))
                 (make-abs (var-of expr) (subst e x (body-of expr)))  ;; [e/x](fun y e1) = (fun y [e/x]e1) (y != x, y not in FV(e1))
             )
         )
      )
      ((app? expr)      ;; [e/x](e1 e2) = ([e/x]e1 [e/x]e2)
         (make-app (subst e x (rator-of expr))
                   (subst e x (rand-of expr))
         )
      )
      ((var? expr)
         (if (eqv? x expr)
             e       ;; [e/x]x = e
             expr    ;; [e/x]y = y
         )
      )
      (else expr)    ;; Error! Just return the expr
    )
)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reduction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define reduce (lambda (expr)
                 (letrec ((reduce? (lambda (x) (cond
                                                 ((abs? x) (reduce? (body-of x)))
                                                 ((app? x) (cond
                                                             ((abs? (rator-of x)) #t)
                                                             ((var? (rator-of x)) (reduce? (rand-of x)))
                                                             ((app? (rator-of x)) (or (reduce? (rator-of x)) (reduce? (rand-of x))))))                    
                                                 (else #f))))
                          (reduce-helper (lambda (e) (cond
                                                      ((and (app? e) (reduce? e)) 
                                                       (cond
                                                         ((var? (rator-of e)) (make-app (rator-of e) (reduce-helper (rand-of e))))
                                                         ((abs? (rator-of e)) (subst (rand-of e) 
                                                                                              (var-of (rator-of e)) 
                                                                                              (body-of (rator-of e))))
                                                         ((app? (rator-of e)) (if (reduce? (rator-of e))
                                                                                   (reduce (make-app (reduce-helper (rator-of e)) (rand-of e)))
                                                                                   (reduce (make-app (rator-of e) (reduce-helper (rand-of e))))))

                                                         (else e)))
                                                      ((abs? e) (make-abs (var-of e) (reduce-helper (body-of e))))
                                                      (else e)))))
                   (if (not (reduce? expr)) expr (reduce (reduce-helper expr))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parser 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define parse-lambda (lambda (expr)
                       (cond
                         ((var? expr) expr)
                         ((null? (cdr expr)) (parse-lambda (car expr)))
                         ((and (>= (length expr) 3)) (cond
                                                       ((eqv? 'fun (car expr)) (make-abs (cadr expr) (parse-lambda (cddr expr))))
                                                       ((eqv? 'fun (cadr expr)) (make-app (parse-lambda (car expr)) (parse-lambda (make-abs (caddr expr) (cdddr expr)))))
                                                       (else (parse-lambda (cons (make-app (parse-lambda (car expr)) 
                                                                                           (parse-lambda (cadr expr))) (cddr expr))))))
                         (else (make-app (parse-lambda (car expr)) (parse-lambda (cdr expr)))))))

(define (interpret E) (reduce (parse-lambda E)))
