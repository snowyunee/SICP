#lang scheme

(require "get_put.scm")
(require "get_put_coercison.scm")
(require "tags.scm")
(require "generic.scm")


(provide make-integer)

(provide make-scheme-number)
(provide make-real)
(provide make-rational)
(provide make-complex-from-real-imag)
(provide make-complex-from-mag-ang)



;(define (add x y) (apply-generic 'add x y))
;(define (sub x y) (apply-generic 'sub x y))
;(define (mul x y) (apply-generic 'mul x y))
;(define (div x y) (apply-generic 'div x y))


(define (square x) (x * x))

; ------------------------------------------------------
; scheme number package
(define (install-integer-package)
  (define (tag x)
    (cons 'integer x))
    ;(attach-tag 'integer x))    
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'integer
       (lambda (x) (tag x)))
  'done-install-integer-package)

(define (make-integer x)
  ((get 'make 'integer) x))



; ------------------------------------------------------
; real package

(define (install-real-package)
  (define (tag x)
    (cons 'real x))
    ;(attach-tag 'real x))    
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real
       (lambda (x) (tag x)))
  'done-install-real-package)

(define (make-real x)
  ((get 'make 'real) x))



; ------------------------------------------------------
; rectangular-package

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done-rectangular)


; ------------------------------------------------------
; polar-package

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done-polar)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))


(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))





;---------------------------------------------------------
; scheme number package

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))    
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number) =)
  (put 'zero? '(scheme-number)
       (lambda (x) (= x 0)))
  'done-scheme-number)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))



;---------------------------------------------------------
; rational package
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (install-rational-package)
  ;; internal procedures
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (x y) (= (* (numer x) (denom y)) (* (numer y) (denom y)))))
  (put 'zero? '(rational)
       (lambda (x) (= (numer x) 0)))
  'done-rational)

(define (make-rational n d)
  ((get 'make 'rational) n d))



;---------------------------------------------------------
; complex package
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(complex complex)
       (lambda (x y) (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y)))))
  (put 'zero? '(complex)
       (lambda (x) (= (magnitude x) 0)))
  ; exercise 2.77
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done-complex)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


;------------------------------------------------------------
; raise

(define (install-raise)
  (define (raise x) (apply-generic 'raise x))
  
  (define (integer->rational n)
    (make-rational n 1))
  (define (rational->real n)
     (make-real (/ (numer n) (denom n))))
  (define (real->complex n)
    (make-complex-from-real-imag n 0))
  
  (put-coercion 'integer   'rational   integer->rational)
  (put-coercion 'rational  'real       rational->real)
  (put-coercion 'real      'complex    real->complex)
  
  (put 'raise '(integer)      integer->rational)
  (put 'raise '(rational)     rational->real)
  (put 'raise '(real)         real->complex)
  'done-raise)


;;------------------------------------------------------------
;; drop
;
;(define (install-drop)
;  (define (drop x) (apply-generic 'drop x))
;  
;  (define (complex->real n)
;    (make-real (real-part n)))
;  (define (real->integer n)
;    (make-integer (round n)))
;  (define (rational->integer n)
;    (make-integer (round (/ (numer (contents n)) (denom (contents))))))
;  
;  (put-coercion 'complex   'real           complex->real)
;  (put-coercion 'real      'integer        real->integer)
;  (put-coercion 'rational  'integer        rational->integer)
;  
;  (put 'drop '(complex)      complex->real)
;  (put 'drop '(real)         real->integer)
;  (put 'drop '(rational)     rational->integer)
;  'done-drop)
;  

;------------------------------------------------------------
; install all

(install-integer-package)
(install-rectangular-package)
(install-polar-package)
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)
(install-real-package)
(install-raise)
;(install-drop)

