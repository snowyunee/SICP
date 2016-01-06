#lang scheme

(require "get_put.scm")
(require "get_put_coercison.scm")
(require "tags.scm")
(require "generic.scm")
(require "numeric.scm")

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (=zero? x) (apply-generic 'zero? x))

(define (make-sparse-terms terms) ((get 'make-terms 'sparse terms)))
(define (make-dense-terms terms)  ((get 'make-terms 'dense-terms terms)))
(define (add-terms x y) (apply-generic 'add-terms x y))
(define (sub-terms x y) (apply-generic 'sub-terms x y))
(define (mul-terms x y) (apply-generic 'mul-terms x y))
(define (zero-terms? x) (apply-generic 'zero-terms? x))


;############################################################
;########## sparse terms              #######################
(define (install-sparse-terms-package)
  (define (add-sparse-terms L1 L2)
    ;(println (list 'add-sparse-terms-L1-L2 'L1 L1 'L2 L2))
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-sparse-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-sparse-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-sparse-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  
  (define (sub-sparse-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (sub-sparse-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (sub-sparse-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (sub (coeff t1) (coeff t2)))
                     (sub-sparse-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  
  (define (mul-sparse-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-sparse-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-sparse-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  
  (define (adjoin-term term term-list)
    (println (list 'adjoin-term 'term term 'term-list term-list))
    (println (list 'coeff (coeff term)))
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  ; e 2.87
  (define (zero-sparse-terms? L)
    (println (list (empty-termlist? L)) )
    (println (list (=zero? (coeff (first-term L))) ))
    (cond ((empty-termlist? L) #t)
          ((not (=zero? (coeff (first-term L)))) #f)
          (else (zero-sparse-terms? rest-terms L))))
  
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'sparse p))
  (put 'add-terms '(sparse sparse) 
       (lambda (p1 p2) (tag (add-sparse-terms p1 p2))))
  (put 'sub-terms '(sparse sparse) 
       (lambda (p1 p2) (tag (sub-sparse-terms p1 p2))))
  (put 'mul-terms '(sparse sparse) 
       (lambda (p1 p2) (tag (mul-sparse-terms p1 p2))))
  (put 'make-terms 'sparse
       (lambda (terms) (tag terms)))
  (put 'zero-terms? '(sparse)
       (lambda (p) (tag (zero-sparse-terms? p))))
  'done-sparse-terms)
  
;############################################################
;############################################################


;############################################################
;########## polynomial package ##############################

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  ;<procedures same-variable? and variable? from section 2.3.2>
  ;; representation of terms and term lists
  ; 2.3.2에서 복사해옴
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  ;<procedures adjoin-term ...coeff from text below>


  ;; continued on next page
  ;;(define (add-poly p1 p2) ...)
  ;<procedures used by add-poly>
  ;(define (mul-poly p1 p2) ...)
  ;<procedures used by mul-poly>
  ; 책 다음 페이지에서 가져옴
  (define (add-poly p1 p2)
    (println p1)
    (println p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))
  
  ; e 2.88
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (sub-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- SUB-POLY"
               (list p1 p2))))
  

  ; e 2.87
  (define (zero? p)
    (zero-terms? (term-list p)))
  
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  ; e 2.87
  (put 'zero? '(polynomial)
       (lambda (p) (zero? p)))
  ; e 2.88
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  'done-install-polynomial)
;############################################################
;############################################################

(install-polynomial-package)
(install-sparse-terms-package)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))


(define p1 (make-polynomial 'x (cons 'sparse (list (list 3 1) (list 2 2) (list 1 1)))))
(define p2 (make-polynomial 'x (cons 'sparse (list (list 3 3) (list 2 3) (list 1 3)))))
;(add p1 p2)


; Exercise 2.87.  Install =zero? for polynomials in the generic arithmetic package. This will allow adjoin-term to work for polynomials with coefficients that are themselves polynomials.
; see commend e 2.87
(println "2.87")
(define pp1 (make-polynomial 'y (cons 'sparse (list (list 3 p1) (list 2 p1) (list 1 1)))))
(define pp2 (make-polynomial 'y (cons 'sparse (list (list 3 p2) (list 2 p2) (list 1 3)))))
(println pp1)
(println pp2)
(add pp1 pp2)

; Exercise 2.88.  Extend the polynomial system to include subtraction of polynomials. (Hint: You may find it helpful to define a generic negation operation.)
(println "2.88")
(sub p1 p2)
(sub pp1 pp2)

; Exercise 2.89 Define procedures that implement the term-list representation described above as appropriate for dense polynomials.

; Exercise 2.90.  Suppose we want to have a polynomial system that is efficient for both sparse and dense polynomials. One way to do this is to allow both kinds of term-list representations in our system. The situation is analogous to the complex-number example of section 2.4, where we allowed both rectangular and polar representations. To do this we must distinguish different types of term lists and make the operations on term lists generic. Redesign the polynomial system to implement this generalization. This is a major effort, not a local change.


; Exercise 2.91.  A univariate polynomial can be divided by another one to produce a polynomial quotient and a polynomial remainder. For example,









