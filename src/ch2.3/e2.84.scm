#lang scheme

(require "get_put.scm")
(require "get_put_coercison.scm")
(require "tags.scm")
(require "generic.scm")
(require "numeric.scm")


;Exercise 2.84.  Using the raise operation of exercise 2.83,
; modify the apply-generic procedure so that it coerces its arguments to have the same type by the method of successive raising,
; as discussed in this section.
; You will need to devise a way to test which of two types is higher in the tower.
; Do this in a manner that is ``compatible'' with the rest of the system and will not lead to problems in adding new levels to the tower.

; generic.scm apply-generic


(put 'sum '(real real real)               (lambda (x y z) (println (list 'sum-real x y z))))
(put 'sum '(integer integer integer)      (lambda (x y z) (println (list 'sum-integer x y z))))
(put 'sum '(rational rational rational)   (lambda (x y z) (println (list 'sum-rational x y z))))
(put 'sum '(complex complex complex)      (lambda (x y z) (println (list 'sum-complex x y z))))

(define (sum x y z) (apply-generic 'sum x y z))   

(sum (make-integer 3) (make-integer 1) (make-integer 1))
(sum (make-integer 3) (make-integer 2) (make-real 1.5))
(sum (make-integer 3) (make-rational 1 2) (make-complex-from-real-imag 1.5 0))

; output
;(sum-integer 3 1 1)
;(sum-real 3 2 1.5)
;(sum-complex (rectangular 3 . 0) (rectangular 1/2 . 0) (rectangular 1.5 . 0))
