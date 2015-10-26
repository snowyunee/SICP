#lang scheme

(require "get_put.scm")
(require "get_put_coercison.scm")
(require "tags.scm")
(require "generic.scm")
(require "numeric.scm")

;Exercise 2.83.  Suppose you are designing a generic arithmetic system for dealing with the tower of types shown in figure 2.25: integer, rational, real, complex.
; For each type (except complex), design a procedure that raises objects of that type one level in the tower.
; Show how to install a generic raise operation that will work for each type (except complex).
; =>


; numeric.scm install-raise

(apply-generic 'raise (make-integer 10))
(apply-generic 'raise (make-rational 1 2))
(apply-generic 'raise (make-real 1.2))


; output
;(rational 10 . 1)
;(real . 1/2)
;(complex rectangular 1.2 . 0)


