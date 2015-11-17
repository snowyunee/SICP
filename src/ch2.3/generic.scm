#lang scheme

(require "tags.scm")
(require "get_put.scm")
(require "get_put_coercison.scm")

(provide apply-generic)


;(define (apply-generic op . args)
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if proc
;          (apply proc (map contents args))
;          (if (= (length args) 2)
;              (let ((type1 (car type-tags))
;                    (type2 (cadr type-tags))
;                    (a1 (car args))
;                    (a2 (cadr args)))
;                (if (equal? type1 type2)
;                  (error "No method for these types"
;                    (list op type-tags))
;                  (let ((t1->t2 (get-coercion type1 type2))
;                        (t2->t1 (get-coercion type2 type1)))
;                    (cond (t1->t2
;                           (apply-generic op (t1->t2 a1) a2))
;                          (t2->t1
;                           (apply-generic op a1 (t2->t1 a2)))
;                          (else
;                           (error "No method for these types"
;                                  (list op type-tags)))))))
;              (error "No method for these types"
;                     (list op type-tags)))))))
;

;---------------------------------------------------------------------
; 2.82

;(define (apply-generic2 op . args)
;  (define (convert type arg)
;    (let ((arg-type (type-tag arg)))
;      (let ((proc (get-coercion arg-type type)))
;        (if (equal? arg-type type)
;            arg
;            (if proc
;                (proc arg)
;                #f)))))
;
;  (define (all-converted? args)
;    (foldr (lambda (v acc) (and acc (pair? v))) #t args))
;
;  (define (convert-all type args)
;    (let ((converted-args (map (lambda (arg) (convert type arg)) args)))
;          (if (all-converted? converted-args)
;              converted-args
;              #f)))
;
;  (define (apply-generic-same-types types args)
;    (if (pair? types)
;        (let ((converted-args (convert-all (car types) args)))
;          (if converted-args
;              (let ((proc (get op (map type-tag converted-args))))
;                (if proc
;                    (apply proc (map contents args))
;                    (apply-generic-same-types (cdr types) args)))
;              (apply-generic-same-types (cdr types) args)))
;        (error "No method for these types" (list op (map type-tag args)))))
;
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if proc
;          (apply proc (map contents args))
;          (apply-generic-same-types type-tags args)))))
;



;---------------------------------------------------------------------
; 2.83

;(define (apply-generic op . args)
;  (define (convert type arg)
;    (let ((arg-type (type-tag arg)))
;      (let ((proc (apply-generic arg-type type)))
;        (if (equal? arg-type type)
;            arg
;            (if proc
;                (let ((converted (proc arg)))
;                  (if (equal? (type-tag converted) type)
;                      converted
;                      (convert type converted)))
;                #f)))))
;
;  (define (all-converted? args)
;    (foldr (lambda (v acc) (and acc (pair? v))) #t args))
;
;  (define (convert-all type args)
;    (let ((converted-args (map (lambda (arg) (convert type arg)) args)))
;          (if (all-converted? converted-args)
;              converted-args
;              #f)))
;
;  (define (apply-generic-same-types types args)
;    (if (pair? types)
;        (let ((converted-args (convert-all (car types) args)))
;          (if converted-args
;              (let ((proc (get op (map type-tag converted-args))))
;                (if proc
;                    (apply proc (map contents args))
;                    (apply-generic-same-types (cdr types) args)))
;              (apply-generic-same-types (cdr types) args)))
;        (error "No method for these types" (list op (map type-tag args)))))
;
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if proc
;          (apply proc (map contents args))
;          (apply-generic-same-types type-tags args)))))


;---------------------------------------------------------------------
; 2.84

(define (apply-generic op . args)
  (define (convert type arg)
    ;(display (list 'convert-type-arg type arg))
    (let ((arg-type (type-tag arg)))
      (let ((proc (get 'raise (list arg-type))))
        (if (equal? arg-type type)
            arg
            (if proc
                (let ((converted (proc (contents arg))))
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
                    (apply proc (map contents converted-args))
                    (apply-generic-same-types (cdr types) args)))
              (apply-generic-same-types (cdr types) args)))
        (error "No method for these types" (list op (map type-tag args)))))

  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (apply-generic-same-types type-tags args)))))



;---------------------------------------------------------------------
; 2.85
;
;(define (apply-generic2 op . args)
;  (define (convert type arg)
;    ;(display (list 'convert-type-arg type arg))
;    (let ((arg-type (type-tag arg)))
;      (let ((proc (get 'raise (list arg-type))))
;        (if (equal? arg-type type)
;            arg
;            (if proc
;                (let ((converted (apply proc arg)))
;                  (if (equal? (type-tag converted) type)
;                      converted
;                      (convert type converted)))
;                #f)))))
;
;  (define (all-converted? args)
;    (foldr (lambda (v acc) (and acc (pair? v))) #t args))
;
;  (define (convert-all type args)
;
;          (if (all-converted? converted-args)
;              converted-args
;              #f))
;
;  (define (apply-generic-same-types types args)
;    (if (pair? types)
;        (let ((converted-args (convert-all (car types) args)))
;          (if converted-args
;              (let ((proc (get op (map type-tag converted-args))))
;                (if proc
;                    (apply proc (map contents args))
;                    (apply-generic-same-types (cdr types) args)))
;              (apply-generic-same-types (cdr types) args)))
;        (error "No method for these types" (list op (map type-tag args)))))
;
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if proc
;          (drop (apply proc (map contents args)))
;          (drop (apply-generic-same-types type-tags args)))))
;
;
