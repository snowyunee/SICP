#lang scheme
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))


;(define (segments->painter segment-list)
;  (lambda (frame)
;    (for-each
;     (lambda (segment)
;       (draw-line
;        ((frame-coord-map frame) (start-segment segment))
;        ((frame-coord-map frame) (end-segment segment))))
;     segment-list)))
;
;(define (make-segment v1 v2)
;  (cons v1 v2))
;(define (start-segment seg)
;  (car seg))
;(define (end-segment seg)
;  (cdr seg))
;(define (outline frame)
;  (list
;   (make-segment (origin-frame frame) (edge1-frame frame))
;   (make-segment (origin-frame frame) (edge2-frame frame))
;   (make-segment (edge1-frame frame) (add-vect (edge1-frame frame) (edge2-frame frame)))
;   (make-segment (edge2-frame frame) (add-vect (edge1-frame frame) (edge2-frame frame)))))
;

(define (outline frame)
  ((segments->painter (list (make-segment origin lower-right)
                            (make-segment lower-right upper-right)
                            (make-segment upper-right upper-left)
                            (make-segment upper-left origin)))
   frame))


(display "exercise 2.49\n")
(paint outline)
;(outline (make-frame (make-vector 
 
