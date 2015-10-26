#lang scheme

(require "get_put.scm")
(require "get_put_coercison.scm")
(require "tags.scm")
;(require "generic.scm")
(require "numeric.scm")


;Exercise 2.84.  Using the raise operation of exercise 2.83,
; modify the apply-generic procedure so that it coerces its arguments to have the same type by the method of successive raising,
; as discussed in this section.
; You will need to devise a way to test which of two types is higher in the tower.
; Do this in a manner that is ``compatible'' with the rest of the system and will not lead to problems in adding new levels to the tower.

; generic.scm apply-generic
(define (apply-generic op . args)
  (define (convert type arg)
    ;(display (list 'convert-type-arg type arg))
    (let ((arg-type (type-tag arg)))
      (let ((proc (get 'raise (list arg-type))))
        (if (equal? arg-type type)
            arg
            (if proc
                (let ((converted (proc arg)))
                  (if (equal? (type-tag converted) type)
                      converted
                      (convert type converted)))
                #f)))))

  (define (all-converted? args)
    (foldr (lambda (v acc) (and acc (pair? v))) #t args))

  (define (convert-all type args)
    (let ((converted-args (map (lambda (arg) (convert type arg)) args)))
          (if (all-converted? converted-args)
              converted-args
              #f)))

  (define (apply-generic-same-types types args)
    (if (pair? types)
        (let ((converted-args (convert-all (car types) args)))
          (if converted-args
              (let ((proc (get op (map type-tag converted-args))))
                (if proc
                    (apply proc (map contents args))
                    (apply-generic-same-types (cdr types) args)))
              (apply-generic-same-types (cdr types) args)))
        (error "No method for these types" (list op (map type-tag args)))))

  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (apply-generic-same-types type-tags args)))))



(put 'sum '(real real real)               (lambda (x y z) (display (list 'sum-real x y z))))
(put 'sum '(integer integer integer)      (lambda (x y z) (display (list 'sum-integer x y z))))
(put 'sum '(rational rational rational)   (lambda (x y z) (display (list 'sum-rational x y z))))
(put 'sum '(complex complex complex)      (lambda (x y z) (display (list 'sum-complex x y z))))

(define (sum x y z) (apply-generic 'sum x y z))   

(sum (make-integer 3) (make-integer 1) (make-integer 1))
(sum (make-integer 3) (make-integer 2) (make-real 1.5))
(sum (make-integer 3) (make-rational 1 2) (make-complex-from-real-imag 1.5 0))

