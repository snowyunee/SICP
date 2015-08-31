#lang scheme

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))


; get / put
(define global-array `())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list (make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))




; e 2.73
(define (deriv exp var)
  ;(print 'exp)
  ;(print exp)
  ;(print '\n)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))
(define (operator exp) (car exp))

(define (operands exp) (cdr exp))


(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (make-exponentiation base e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) base)
        (else (list '** base e))))



;(deriv '(** (+ x y) 2) 'x)
;(deriv '(* a (** x 2)) 'x)
;(deriv '(+ (* a (** x 2)) (* b x)) 'x)
;(deriv '(+ (+ (* a (** x 2)) (* b x)) c) 'x)
;(* 2 (+ x y))
;(* a (* 2 x))
;(+ (* a (* 2 x)) b)
;(+ (* a (* 2 x)) b)

;a.  Explain what was done above. Why can't we assimilate the predicates number? and same-variable? into the data-directed dispatch?
; => 

;b.  Write the procedures for derivatives of sums and products, and the auxiliary code required to install them in the table used by the program above.
(define (install-sum-deriv)
  ;; internal procedures
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))

  ;; interface to the rest of the system
  (put 'deriv '+ deriv-sum)
  'done-sum)

(define (install-product-deriv)
  ;; internal procedures
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  (define (deriv-product exp var)
      ;(print 'product-exp)
      ;(print exp)
      ;(print '\n)
    (make-sum 
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))
      ))

  ;; interface to the rest of the system
  (put 'deriv '* deriv-product)
  'done-product)

(install-sum-deriv)
(install-product-deriv)
(print global-array)

(deriv '(* 2 (+ x y)) 'x)
(deriv '(* a (* 2 x)) 'x)
(deriv '(+ (* a (* 2 x)) b) 'x)
(deriv '(+ (* a (* 2 x)) b) 'x)

;c.  Choose any additional differentiation rule that you like, such as the one for exponents (exercise 2.56), and install it in this data-directed system.

;2.56
(define (install-expo-deriv)
  ;; internal procedures
  (define (base p) (car p))
  (define (exponent p) (cadr p))
  (define (deriv-exponentiation exp var)
      ;(print 'product-exp)
      ;(print exp)
      ;(print '\n)
    (make-product  (make-product (exponent exp)
                                 (make-exponentiation (base exp)
                                                      (make-sum (exponent exp) -1)))
                   (deriv (base exp) var)))

  ;; interface to the rest of the system
  (put 'deriv '** deriv-exponentiation)
  'done-exponentiation)

(deriv '(** (+ x y) 2) 'x)
(deriv '(* a (** x 2)) 'x)
(deriv '(+ (* a (** x 2)) (* b x)) 'x)
(deriv '(+ (+ (* a (** x 2)) (* b x)) c) 'x)

;d.  In this simple algebraic manipulator the type of an expression is the algebraic operator that binds it together. Suppose, however, we indexed the procedures in the opposite way, so that the dispatch line in deriv looked like

;((get (operator exp) 'deriv) (operands exp) var)

;What corresponding changes to the derivative system are required?

