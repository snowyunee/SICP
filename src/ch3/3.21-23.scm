#lang planet neil/sicp

;(define (front-ptr queue) (car queue))
;(define (rear-ptr queue) (cdr queue))
;(define (set-front-ptr! queue item) (set-car! queue item))
;(define (set-rear-ptr! queue item) (set-cdr! queue item))
;
;(define (empty-queue? queue) (null? (front-ptr queue)))
;
;(define (make-queue) (cons '() '()))
;
;(define (front-queue queue)
;  (if (empty-queue? queue)
;        (error "FRONT called with an empty queue" queue)
;              (car (front-ptr queue))))
;
;(define (insert-queue! queue item)
;  (let ((new-pair (cons item '())))
;      (cond ((empty-queue? queue)
;             (set-front-ptr! queue new-pair) (set-rear-ptr! queue new-pair)
;             queue)
;            (else
;              (set-cdr! (rear-ptr queue) new-pair)
;              (set-rear-ptr! queue new-pair)
;              queue))))
;
;(define (delete-queue! queue)
;  (cond ((empty-queue? queue)
;         (error "DELETE! called with an empty queue" queue))
;        (else
;          (set-front-ptr! queue (cdr (front-ptr queue)))
;          queue)))
;
;Exercise 3.21.  Ben Bitdiddle decides to test the queue implementation described above.
; He types in the procedures to the Lisp interpreter and proceeds to try them out:
;
;(define q1 (make-queue))
;(insert-queue! q1 'a)
;((a) a)
;(insert-queue! q1 'b)
;((a b) b)
;(delete-queue! q1)
;((b) b)
;(delete-queue! q1)
;(() b)
;
;``It's all wrong!'' he complains. ``The interpreter's response shows that the last item is inserted into the queue twice. And when I delete both items, the second b is still there, so the queue isn't empty, even though it's supposed to be.'' Eva Lu Ator suggests that Ben has misunderstood what is happening. ``It's not that the items are going into the queue twice,'' she explains. ``It's just that the standard Lisp printer doesn't know how to make sense of the queue representation. If you want to see the queue printed correctly, you'll have to define your own print procedure for queues.'' Explain what Eva Lu is talking about. In particular, show why Ben's examples produce the printed results that they do. Define a procedure print-queue that takes a queue as input and prints the sequence of items in the queue.


; (define (print-queue q)
;   (display (front-ptr q))
;   (newline))
 
; (define q (make-queue))
; (print-queue (insert-queue! q 'a))
; ;(a)
; (print-queue (insert-queue! q 'b))
; ;(a b)
; (print-queue (insert-queue! q 'c))
; ;(a b c)
; (print-queue (insert-queue! q 'd))
; ;(a b c d)
;  
; (define q1 (make-queue))
; (print-queue (insert-queue! q1 'a))
; ;(a)
; (print-queue (insert-queue! q1 'b))
; ;(a b)
; (print-queue (delete-queue! q1))
; ;(b)
; (print-queue (delete-queue! q1))
; ;()


;Exercise 3.22.  Instead of representing a queue as a pair of pointers, we can build a queue as a procedure with local state. The local state will consist of pointers to the beginning and the end of an ordinary list. Thus, the make-queue procedure will have the form
;
;(define (make-queue)
;  (let ((front-ptr ...)
;        (rear-ptr ...))
;    <definitions of internal procedures>
;    (define (dispatch m) ...)
;    dispatch))
;
;Complete the definition of make-queue and provide implementations of the queue operations using this representation.

(define (make-queue) 
  (let ((front-ptr '()) 
        (rear-ptr '())) 
    (define (emtpy-queue?) (null? front-ptr)) 
    (define (set-front-ptr! item) (set! front-ptr item)) 
    (define (set-rear-ptr! item) (set! rear-ptr item)) 
    (define (front-queue)  
      (if (emtpy-queue?) 
        (error "FRONT called with an empty queue") 
        (car front-ptr))) 
    (define (insert-queue! item) 
      (let ((new-pair (cons item '()))) 
        (cond ((emtpy-queue?) 
               (set-front-ptr! new-pair) 
               (set-rear-ptr! new-pair)) 
              (else  
                (set-cdr! rear-ptr new-pair) 
                (set-rear-ptr! new-pair))))) 
    (define (delete-queue!) 
      (cond ((emtpy-queue?) 
             (error "DELETE called with an emtpy queue")) 
            (else (set-front-ptr! (cdr front-ptr)))))

    (define (print-queue) (display front-ptr))

    (define (dispatch m) 
      (cond ((eq? m 'empty-queue) emtpy-queue?) 
            ((eq? m 'front-queue) front-queue) 
            ((eq? m 'insert-queue!) insert-queue!) 
            ((eq? m 'delete-queue!) delete-queue!) 
            ((eq? m 'print-queue) print-queue) 
            (else (error "undefined operation -- QUEUE" m)))) 
    dispatch)) 


(define (empty-queue? q) ((q 'empty-queue?)))
(define (front-queue q) ((q 'front-queue)))
(define (insert-queue! q i) ((q 'insert-queue!) i) q)
(define (delete-queue! q) ((q 'delete-queue!)) q)
(define (print-queue q) ((q 'print-queue)))


(define q (make-queue))
(print-queue (insert-queue! q 'a))
;(a)
(print-queue (insert-queue! q 'b))
;(a b)
(print-queue (insert-queue! q 'c))
;(a b c)
(print-queue (insert-queue! q 'd))
;(a b c d)
 
(define q1 (make-queue))
(print-queue (insert-queue! q1 'a))
;(a)
(print-queue (insert-queue! q1 'b))
;(a b)
(print-queue (delete-queue! q1))
;(b)
(print-queue (delete-queue! q1))
;()

;Exercise 3.23.  A deque (``double-ended queue'') is a sequence in which items can be inserted and deleted at either the front or the rear.
; Operations on deques are the constructor make-deque, the predicate empty-deque?,
; selectors front-deque and rear-deque, and mutators front-insert-deque!, rear-insert-deque!, front-delete-deque!, and rear-delete-deque!.
; Show how to represent deques using pairs, and give implementations of the operations.
; All operations should be accomplished in (1) steps.


 (define (make-deque) (cons '() '()))
 (define (front-ptr deque) (car deque)) 
 (define (rear-ptr deque) (cdr deque))
 (define (empty-deque? deque) (null? (front-ptr deque)))
 (define (set-front! deque item) (set-car! deque item))
 (define (set-rear! deque item) (set-cdr! deque item))
  
 (define (get-item deque end)
   (if (empty-deque? deque)
     (error "Trying to retrieve item from empty deque" deque)
     (caar (end deque))))
  
 (define (insert-deque! deque item end)
   (let ((new-pair (cons (cons item nil) nil)))
     (cond ((empty-deque? deque)
            (set-front! deque new-pair)
            (set-rear! deque new-pair))
           ((eq? end 'front)
            (set-cdr! new-pair (front-ptr deque))
            (set-cdr! (car (front-ptr deque)) new-pair)
            (set-front! deque new-pair))
           (else (set-cdr! (rear-ptr deque) new-pair)
                 (set-cdr! (car new-pair) (rear-ptr deque))
                 (set-rear! deque new-pair)))))
  
 (define (front-delete-deque! deque) 
   (cond ((empty-deque? deque) (error "Cannot delete from empty deque" deque)) 
         (else (set-front! deque (cdr (front-ptr deque))) 
               (or (empty-deque? deque) (set-cdr! (car (front-ptr deque)) nil))))) 
  
 (define (rear-delete-deque! deque) 
   (cond ((empty-deque? deque) (error "Cannot delete from empty deque" deque)) 
         (else (set-rear! deque (cdar (rear-ptr deque))) 
               (if (null? (rear-ptr deque)) (set-front! deque nil) 
                 (set-cdr! (rear-ptr deque) nil))))) 
  
 (define (front-insert-deque! deque item) (insert-deque! deque item 'front)) 
 (define (rear-insert-deque! deque item) (insert-deque! deque item 'rear)) 
 (define (front-deque deque) (get-item deque front-ptr)) 
 (define (rear-deque deque) (get-item deque rear-ptr)) 
  
 (define (print-deque d) 
   (define (iter res _d) 
     (if (or (null? _d) (empty-deque? _d)) res 
       (iter (append res (list (caaar _d))) (cons (cdar _d) (cdr d))))) 
   (iter nil d)) 


(define deq (make-deque))
(print-deque deq)
(print-deque (front-insert-deque! deq 'a))
;(a)
(print-deque (front-insert-deque! deq 'b))
;(b a)
(print-deque (front-insert-deque! deq 'c))
;(c b a)
(print-deque (front-insert-deque! deq 'd))
;(d c b a)
 
(define deq1 (make-queue))
(print-deque (front-insert-deque! deq1 'a))
;(a)                       
(print-deque (front-insert-deque! deq1 'b))
;(a b)                     
(print-deque (front-delete-deque! deq1))
;(b)                       
(print-deque (front-delete-deque! deq1))
;()

