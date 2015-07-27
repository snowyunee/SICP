#lang scheme

;2.61
(define (adjoin-set x set)
  (cond ((= x (car set)) set)
        ((< x (car set)) (adjoin-set x (cdr set)))
        (else (cons x set))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))


(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))


;2.62
(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1 (union-set (cdr set1) (cdr set2))))
                 ((< x1 x2)
                  (cons (car set1) (union-set (cdr set1) set2)))
                 ((< x2 x1)
                  (cons (car set2) (union-set set1 (cdr set2)))))))))


(intersection-set (list 1 2 3) (list 2 3 4))

(union-set (list 1 2 3) (list 2 3 4))
(union-set (list 1 2 3 5) (list 2 3 4))
