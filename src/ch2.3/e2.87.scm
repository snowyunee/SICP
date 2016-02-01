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
(define (div-terms x y) (apply-generic 'div-terms x y))
(define (zero-terms? x) (apply-generic 'zero-terms? x))


;############################################################
;########## sparse terms              #######################
(define (install-sparse-terms-package)
  (define (add-sparse-terms L1 L2)
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

  (define (div-sparse-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                       (div-sparse-terms (sub-sparse-terms L1 (mul-sparse-terms (list (make-term new-o new-c)) L2)) L2)
                       ))
                  (list (adjoin-term (make-term new-o new-c) (car rest-of-result))
                        (cadr rest-of-result))
                  ))))))
  
  (define (adjoin-term term term-list)
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
  (put 'div-terms '(sparse sparse)
       (lambda (p1 p2) (tag (div-sparse-terms p1 p2))))
  (put 'make-terms 'sparse
       (lambda (terms) (tag terms)))
  (put 'zero-terms? '(sparse)
       (lambda (p) (zero-sparse-terms? p)))
  'done-sparse-terms)
  
;############################################################
;############################################################



;############################################################
;########## dense terms              ########################
(define (install-dense-terms-package)
  (define (add-dense-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-dense-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-dense-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-dense-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  
  (define (sub-dense-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (sub-dense-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (sub-dense-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (sub (coeff t1) (coeff t2)))
                     (sub-dense-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  
  (define (mul-dense-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-dense-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-dense-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  
  (define (adjoin-term term term-list)
    (cond ((=zero? (coeff term)) term-list)
          ((equal? (order term) (+ 1 (length term-list))) (cons (coeff term) term-list))
          (else (adjoin-term term (cons 0 term-list)))))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (make-term (length term-list) (car term-list)))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  ; e 2.87
  (define (zero-dense-terms? L)
    (cond ((empty-termlist? L) #t)
          ((not (=zero? (coeff (first-term L)))) #f)
          (else (zero-dense-terms? rest-terms L))))
  
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'dense p))
  (put 'add-terms '(dense dense) 
       (lambda (p1 p2) (tag (add-dense-terms p1 p2))))
  (put 'sub-terms '(dense dense) 
       (lambda (p1 p2) (tag (sub-dense-terms p1 p2))))
  (put 'mul-terms '(dense dense) 
       (lambda (p1 p2) (tag (mul-dense-terms p1 p2))))
  (put 'make-terms 'dense
       (lambda (terms) (tag terms)))
  (put 'zero-terms? '(dense)
       (lambda (p) (zero-dense-terms? p)))
  'done-dense-terms)
  
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
  ;(define (add-poly p1 p2)
  ;  (if (same-variable? (variable p1) (variable p2))
  ;      (make-poly (variable p1)
  ;                 (add-terms (term-list p1)
  ;                            (term-list p2)))
  ;      (error "Polys not in same var -- ADD-POLY"
  ;             (list p1 p2))))
  
  ; e 2.92
  (define (add-poly p1 p2)
    (println (list p1 p2))
    (println (list (variable p1) (variable p2)))
    (cond ((same-variable? (variable p1) (variable p2))
           (make-poly (variable p1)
                      (add-terms (term-list p1)
                                 (term-list p2))))
          ((< (order (variable p1)) (order (variable p2)))
           (add-poly (make-poly (variable p2) (cons 'sparse (list (list 0 p1)))) p2))
          (else 
           (add-poly (make-poly (variable p1) (cons 'sparse (list (list 0 p2)))) p1))))

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

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (div-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- DIV-POLY"
               (list p1 p2))))

  ; e 2.92
  (define (order variable)
    (cond ((same-variable? variable 'x) 0)
          ((same-variable? variable 'y) 1)
          ((same-variable? variable 'z) 2)
          ))
    

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
  ; e 2.91
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (tag (div-poly p1 p2))))
  'done-install-polynomial)
;############################################################
;############################################################

(install-polynomial-package)
(install-sparse-terms-package)
(install-dense-terms-package)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))


(define p1 (make-polynomial 'x (cons 'sparse (list (list 5 1) (list 2 2) (list 1 1)))))
(define p2 (make-polynomial 'x (cons 'sparse (list (list 5 3) (list 2 3) (list 1 3)))))
(add p1 p2)


; Exercise 2.87.  Install =zero? for polynomials in the generic arithmetic package. This will allow adjoin-term to work for polynomials with coefficients that are themselves polynomials.
; see commend e 2.87
(println "2.87")
(define pp1 (make-polynomial 'y (cons 'sparse (list (list 5 p1) (list 2 p1) (list 1 1)))))
(define pp2 (make-polynomial 'y (cons 'sparse (list (list 5 p2) (list 2 p2) (list 1 3)))))
(println pp1)
(println pp2)
(add pp1 pp2)

; Exercise 2.88.  Extend the polynomial system to include subtraction of polynomials. (Hint: You may find it helpful to define a generic negation operation.)
(println "2.88")
(sub p1 p2)
(sub pp1 pp2)

; Exercise 2.89 Define procedures that implement the term-list representation described above as appropriate for dense polynomials.
; Exercise 2.90.  Suppose we want to have a polynomial system that is efficient for both sparse and dense polynomials. One way to do this is to allow both kinds of term-list representations in our system. The situation is analogous to the complex-number example of section 2.4, where we allowed both rectangular and polar representations. To do this we must distinguish different types of term lists and make the operations on term lists generic. Redesign the polynomial system to implement this generalization. This is a major effort, not a local change.
(println "2.89, 2.90")
(define dp1 (make-polynomial 'x (cons 'dense (list 1 0 0 2 1))))
(define dp2 (make-polynomial 'x (cons 'dense (list 3 0 0 3 3))))
(define dpp1 (make-polynomial 'y (cons 'dense (list dp1 0 0 dp1 1))))
(define dpp2 (make-polynomial 'y (cons 'dense (list dp2 0 0 dp2 3))))
(println dpp1)
(println dpp2)
(add dpp1 dpp2)


; Exercise 2.91.  A univariate polynomial can be divided by another one to produce a polynomial quotient and a polynomial remainder. For example,
;
;(x^5 - 1) / (x^2 - 1) = x^3 + x remainter = x - 1
;
;Division can be performed via long division. That is, divide the highest-order term of the dividend by the highest-order term of the divisor. The result is the first term of the quotient. Next, multiply the result by the divisor, subtract that from the dividend, and produce the rest of the answer by recursively dividing the difference by the divisor. Stop when the order of the divisor exceeds the order of the dividend and declare the dividend to be the remainder. Also, if the dividend ever becomes zero, return zero as both quotient and remainder.
;We can design a div-poly procedure on the model of add-poly and mul-poly. The procedure checks to see if the two polys have the same variable. If so, div-poly strips off the variable and passes the problem to div-terms, which performs the division operation on term lists. Div-poly finally reattaches the variable to the result supplied by div-terms. It is convenient to design div-terms to compute both the quotient and the remainder of a division. Div-terms can take two term lists as arguments and return a list of the quotient term list and the remainder term list.
;Complete the following definition of div-terms by filling in the missing expressions. Use this to implement div-poly, which takes two polys as arguments and returns a list of the quotient and remainder polys.
(println "2.91")
(define k1 (make-polynomial 'x (cons 'sparse (list (list 5 1) (list 0 -1)))))
(define k2 (make-polynomial 'x (cons 'sparse (list (list 2 1) (list 0 -1)))))
(div k1 k2)

(println "2.92")
(define   x1 (make-polynomial 'x (cons 'sparse (list (list 1 1)))))
(define   y1 (make-polynomial 'y (cons 'sparse (list (list 1 1)))))
(define   z1 (make-polynomial 'z (cons 'sparse (list (list 1 1)))))
(define  xy1 (make-polynomial 'y (cons 'sparse (list (list 1 x1)))))
(define  xz1 (make-polynomial 'z (cons 'sparse (list (list 1 x1)))))
(define xyz1 (make-polynomial 'z (cons 'sparse (list (list 1 xy1)))))
(println (list "add " x1 y1))
(add x1 y1)
(add xy1 xz1)
(add xyz1 xyz1)


; output
;(polynomial x sparse (3 (scheme-number . 4)) (2 (scheme-number . 5)) (1 (scheme-number . 4)))
;"2.87"
;(polynomial y sparse (3 (polynomial x sparse (3 1) (2 2) (1 1))) (2 (polynomial x sparse (3 1) (2 2) (1 1))) (1 1))
;(polynomial y sparse (3 (polynomial x sparse (3 3) (2 3) (1 3))) (2 (polynomial x sparse (3 3) (2 3) (1 3))) (1 3))
;(polynomial y sparse (3 (polynomial x sparse (3 (scheme-number . 4)) (2 (scheme-number . 5)) (1 (scheme-number . 4)))) (2 (polynomial x sparse (3 (scheme-number . 4)) (2 (scheme-number . 5)) (1 (scheme-number . 4)))) (1 (scheme-number . 4)))
;"2.88"
;(polynomial x sparse (3 (scheme-number . -2)) (2 (scheme-number . -1)) (1 (scheme-number . -2)))
;(polynomial y sparse (3 (polynomial x sparse (3 (scheme-number . -2)) (2 (scheme-number . -1)) (1 (scheme-number . -2)))) (2 (polynomial x sparse (3 (scheme-number . -2)) (2 (scheme-number . -1)) (1 (scheme-number . -2)))) (1 (scheme-number . -2)))
;"2.89, 2.90"
;(polynomial y dense (polynomial x dense 1 2 1) (polynomial x dense 1 2 1) 1)
;(polynomial y dense (polynomial x dense 3 3 3) (polynomial x dense 3 3 3) 3)
;(polynomial y dense (polynomial x dense (scheme-number . 4) (scheme-number . 5) (scheme-number . 4)) (polynomial x dense (scheme-number . 4) (scheme-number . 5) (scheme-number . 4)) (scheme-number . 4))
;"2.91"
;(polynomial x sparse ((3 (scheme-number . 1)) (1 (scheme-number . -1))) ((1 (scheme-number .  (0 -1)))


