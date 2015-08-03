#lang scheme
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))


(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))


(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define tree1 (adjoin-set 7 (adjoin-set 6 (adjoin-set 5 (adjoin-set 4 (adjoin-set 3 (adjoin-set 2 (adjoin-set 1 '()))))))))
;(define tree2 (adjoin-set 5 (adjoin-set 3 (adjoin-set 7 (adjoin-set 2 (adjoin-set 4 (adjoin-set 6 (adjoin-set 9 '()))))))))

(define tree2-16-1 (adjoin-set 7 (adjoin-set 3 (adjoin-set 9 (adjoin-set 1 (adjoin-set 5 (adjoin-set 11 '())))))))
(define tree2-16-2 (adjoin-set 3 (adjoin-set 1 (adjoin-set 7 (adjoin-set 5 (adjoin-set 9 (adjoin-set 11 '())))))))
(define tree2-16-3 (adjoin-set 5 (adjoin-set 3 (adjoin-set 1 (adjoin-set 9 (adjoin-set 7 (adjoin-set 11 '())))))))


; 2.63
(display "origin\n")
(print tree1)
(display "\n")
(print tree2-16-1 )
(display "\n")
(print tree2-16-2 )
(display "\n")
(print tree2-16-3 )

(display "\ntree->list-1\n")
(print (tree->list-1 tree1))
(display "\n")
(print (tree->list-1 tree2-16-1))
(display "\n")
(print (tree->list-1 tree2-16-2))
(display "\n")
(print (tree->list-1 tree2-16-3))
(display "\ntree->list-2\n")
(print (tree->list-2 tree1))
(display "\n")
(print (tree->list-2 tree2-16-1))
(display "\n")
(print (tree->list-2 tree2-16-2))
(display "\n")
(print (tree->list-2 tree2-16-3))

;(display "\ntest\n")
;(print (tree->list-1 (right-branch tree1)))
;(display "\ntest\n")
;(print (append '() (cons 1 (append '() (cons 2 (list ))))))
;(display "\ntest\n")
;(print (cons 2 (list )))

  
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))


; 2.64
(display "(list->tree (list 1 3 5 7 9 11) )\n")

(print (list->tree (list 1 3 5 7 9 11) ))




; ---------------------- 2.65

(define (intersection-ordered-list set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-ordered-list (cdr set1)
                                                (cdr set2))))
              ((< x1 x2)
               (intersection-ordered-list (cdr set1) set2))
              ((< x2 x1)
               (intersection-ordered-list set1 (cdr set2)))))))


(define (union-ordered-list set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1 (union-ordered-list (cdr set1) (cdr set2))))
                 ((< x1 x2)
                  (cons (car set1) (union-ordered-list (cdr set1) set2)))
                 ((< x2 x1)
                  (cons (car set2) (union-ordered-list set1 (cdr set2)))))))))


(define (union-set2 t1 t2)
  (let ((l1 (tree->list-2 t1))
        (l2 (tree->list-2 t2)))
    (let ((ul (union-ordered-list l1 l2)))
      (list->tree ul))))
  
(define t-11 (adjoin-set 7 (adjoin-set 3 (adjoin-set 9 (adjoin-set 1 (adjoin-set 5 (adjoin-set 11 '())))))))
(define t-12 (adjoin-set 3 (adjoin-set 1 (adjoin-set 7 (adjoin-set 5 (adjoin-set 9 (adjoin-set 11 '())))))))
(display "\nt-11\n")
(print (tree->list-2 t-11))
(display "\nt-12\n")
(print (tree->list-2 t-12))
(display "\nunion-set2\n")
(print (union-set2 t-11 t-12))
;(print (tree->list-2 (union-set2 t-11 t-12 (length t-11) (length t-12))))

(define (intersection-set2 t1 t2)
  (let ((l1 (tree->list-2 t1))
        (l2 (tree->list-2 t2)))
    (let ((ul (intersection-ordered-list l1 l2)))
      (list->tree ul))))

(display "\nintersection-set2\n")
(print (intersection-set2 t-11 t-12))

; ---------------------- 2.66

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        (else
         (let ((r-key (entry set-of-records)))
           (cond ((= given-key r-key) (entry set-of-records))
                 ((< given-key r-key) (lookup given-key (left-branch set-of-records)))
                 (else (lookup given-key (left-branch set-of-records)))
                 )))))
(display "\nlookup\n")
;(define t-11-1 (adjoin-set (cons 7 7) (adjoin-set (cons 3 3) (adjoin-set (cons 9 9) (adjoin-set (cons 1 1) (adjoin-set (cons 5 5) (adjoin-set (cons 11 11) '())))))))
;(print t-11-1)
(print (lookup 1 t-11))
(print (lookup 100 t-11))