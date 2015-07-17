#lang scheme




(define (variable? x) (symbol? x))


(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))


;(define (make-sum a1 a2) (list '+ a1 a2))

;(define (make-product m1 m2) (list '* m1 m2))


(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))


(define (addend s) (cadr s))


(define (augend s) (caddr s))


(define (product? x)
  (and (pair? x) (eq? (car x) '*)))


(define (multiplier p) (cadr p))


(define (multiplicand p) (caddr p))



(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))


;(deriv '(+ x 3) 'x)
;(deriv '(* x y) 'x)
;(deriv '(* (* x y) (+ x 3)) 'x)


;2.56
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base s) (cadr s))
(define (exponent s) (caddr s))

(define (make-exponentiation base e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) base)
        (else (list '** base e))))




(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((exponentiation? exp)
         (make-product
          (make-product
           (exponent exp)
           (make-exponentiation
            (base exp)
            (make-sum (exponent exp) -1)))
          (deriv (base exp) var)))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))


(deriv '(** (+ x y) 2) 'x)
(deriv '(* a (** x 2)) 'x)
(deriv '(+ (* a (** x 2)) (* b x)) 'x)
(deriv '(+ (+ (* a (** x 2)) (* b x)) c) 'x)
;(* 2 (+ x y))
;(* a (* 2 x))
;(+ (* a (* 2 x)) b)
;(+ (* a (* 2 x)) b)

