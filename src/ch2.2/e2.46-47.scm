#lang scheme


(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

; exercise 2.46
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))


(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))


;((frame-coord-map a-frame) (make-vect 0 0))
;(origin-frame a-frame)

(define-values (s v v1 v2) (values 10 (cons 3 5) (cons 9 10) (cons 7 9)))


(display "exercise 2.46\n")
(make-vect 5 7)
(xcor-vect v)
(ycor-vect v)
(add-vect v1 v2)
(sub-vect v1 v2)
(scale-vect s v)



; exercise 2.47
;(define (make-frame origin edge1 edge2)
;  (list origin edge1 edge2))
;(define (origin-frame frame)
;  (car frame))
;(define (edge1-frame frame)
;  (cadr frame))
;(define (edge2-frame frame)
;  (caddr frame))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (car (cdr frame)))
(define (edge2-frame frame)
  (cdr (cdr frame)))


(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect  (xcor-vect v)
                            (edge1-frame frame))
               (scale-vect  (ycor-vect v)
                            (edge2-frame frame))))))

(define-values (ori e1 e2) (values (make-vect 10 10) (make-vect 20 20) (make-vect 10 15)))
(define-values (f) (values (make-frame ori e1 e2)))



(display "exercise 2.47\n")
(display "origin: ")(origin-frame f)
(display "edge1: ")(edge1-frame f)
(display "edge2: ")(edge2-frame f)
((frame-coord-map f) (cons 0 0))
((frame-coord-map f) (cons 1 1))
((frame-coord-map f) (cons 0.5 0.5))

; exec
;exercise 2.46
;(5 . 7)
;3
;5
;(16 . 19)
;(2 . 1)
;(30 . 50)
;exercise 2.47
;origin: (10 . 10)
;edge1: (20 . 20)
;edge2: (10 . 15)
;(10 . 10)
;(40 . 45)
;(25.0 . 27.5)


;(define (segments->painter segment-list)
;  (lambda (frame)
;    (for-each
;     (lambda (segment)
;       (draw-line
;        ((frame-coord-map frame) (start-segment segment))
;        ((frame-coord-map frame) (end-segment segment))))
;     segment-list)))

(define (make-segment v1 v2)
  (cons v1 v2))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

(make-segment v1 v2)
(start-segment (make-segment v1 v2))
(end-segment (make-segment v1 v2))



 
