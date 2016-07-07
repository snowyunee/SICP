#lang planet neil/sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;(define count-pairs
;  (let ((seen nil))
;    (lambda (x)
;      (cond ((not (pair? x)) 0)
;            ((memq x seen) 0)
 ;           (else (set! seen (cons x seen))
;                  (+ (count-pairs (car x))
;                     (count-pairs (cdr x))
;                     1))))))
 

(define l31 (list 'a 'b 'c))
 
(define l41 (list 'b 'c))
(define l42 (list 'a))
(set-car! l41 l42)
(set-car! (cdr l41) l42)
 
(define l71 (list 'c))
(define l72 (list 'b))
(define l73 (list 'a))
(set-car! l72 l73)
(set-cdr! l72 l73)
(set-car! l71 l72)
(set-cdr! l71 l72)
 
(define linf (list 'a 'b 'c))
(set-cdr! (cdr (cdr linf)) linf)
 
(count-pairs l31)
3
(count-pairs l41)
4
(count-pairs (car l71))
7



