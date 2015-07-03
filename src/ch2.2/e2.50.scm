 #lang scheme    
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1))) 


;(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))


(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))


(define (transform-painter2 painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))


(define (flip-vert2 painter)
  (transform-painter2 painter
                     (make-vect 0.0 1.0)   ; new origin
                     (make-vect 1.0 1.0)   ; new end of edge1
                     (make-vect 0.0 0.0))) ; new end of edge2


(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))


(define (rotate-90 painter)
  (transform-painter2 painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))


(define (squash-inwards2 painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(define (beside2 painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter2 painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter2 painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))



; exercise 2.50
(define (flip-horiz2 painter)
  (transform-painter2 painter
                     (make-vect 1.0 0.0)   ; new origin
                     (make-vect 0.0 0.0)   ; new end of edge1
                     (make-vect 1.0 1.0))) ; new end of edge2


(define (rotate-180 painter)
  (transform-painter2 painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate-270 painter)
  (transform-painter2 painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))


; exercise 2.51z
(define (below2 painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-top
           (transform-painter2 painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-bottom
           (transform-painter2 painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(define (below3 painter1 painter2)
  (rotate-90 (beside2 (rotate-270 painter1) (rotate-270 painter2))))

;(paint einstein)
;(display "flip-vert2")
;(paint (flip-vert2 einstein))
;(display "flip-horiz2")
;(paint (flip-horiz2 einstein))
;(display "rotate-180")
;(paint (rotate-180 einstein))
;(display "rotate-270")
;(paint (rotate-270 einstein))
(define (test painter)
           (transform-painter2 painter
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              (make-vect 0.0 0.5)))


(paint (rotate-90 einstein))
(paint (rotate-180 einstein))
(paint (rotate-270 einstein))

(display "below2")
(paint (below2 einstein einstein))
(display "below3")
(paint (below3 einstein einstein))