#lang scheme

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))


(define wave-segments
 (list
  (make-segment
   (make-vect 0.006 0.840)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.006 0.635)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.403 0.646))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.354 0.492))
  (make-segment
   (make-vect 0.403 0.646)
   (make-vect 0.348 0.845))
  (make-segment
   (make-vect 0.354 0.492)
   (make-vect 0.249 0.000))
  (make-segment
   (make-vect 0.403 0.000)
   (make-vect 0.502 0.293))
  (make-segment
   (make-vect 0.502 0.293)
   (make-vect 0.602 0.000))
  (make-segment
   (make-vect 0.348 0.845)
   (make-vect 0.403 0.999))
  (make-segment
   (make-vect 0.602 0.999)
   (make-vect 0.652 0.845))
  (make-segment
   (make-vect 0.652 0.845)
   (make-vect 0.602 0.646))
  (make-segment
   (make-vect 0.602 0.646)
   (make-vect 0.751 0.646))
  (make-segment
   (make-vect 0.751 0.646)
   (make-vect 0.999 0.343))
  (make-segment
   (make-vect 0.751 0.000)
   (make-vect 0.597 0.442))
  (make-segment
   (make-vect 0.597 0.442)
   (make-vect 0.999 0.144))
  (make-segment ; smile
   (make-vect 0.376 0.746)
   (make-vect 0.460 0.790))))


(define wave (segments->painter wave-segments))

; 2.52 a
(paint wave)

; 2.52 b
;(define (split f1 f2)
;  (lambda (painter n)
;    (if (= n 0)
;        painter
;        (let ((smaller (split painter (- n 1))))
;          (f1 painter (f2 smaller smaller))))))
;
;
;(define right-split (split beside below))
;(define up-split (split below beside))
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))
(define (corner-split painter n)
 (if (= n 0)
     painter
     (let ((up (up-split painter (- n 1)))
           (right (right-split painter (- n 1)))
           (corner (corner-split painter (- n 1))))
         (beside (below painter up)
                 (below right corner)))))

(paint (corner-split wave 2))

(define (squre-limit painter n)
	(let ((quarter (rotate180 (corner-split painter n))))
		(let ((half (beside (flip-horiz quarter) quarter)))
  		(below (flip-vert half) half))))


