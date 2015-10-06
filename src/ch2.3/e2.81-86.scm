#lang scheme

(require "get_put.scm")
(require "get_put_coercison.scm")
(require "tags.scm")
  



;Exercise 2.81.  Louis Reasoner has noticed that apply-generic may try to coerce the arguments to each other's type even if they already have the same type. Therefore, he reasons, we need to put procedures in the coercion table to "coerce" arguments of each type to their own type. For example, in addition to the scheme-number->complex coercion shown above, he would do:
;
;(define (scheme-number->scheme-number n) n)
;(define (complex->complex z) z)
;(put-coercion 'scheme-number 'scheme-number
;              scheme-number->scheme-number)
;(put-coercion 'complex 'complex complex->complex)
;
;a. With Louis's coercion procedures installed, what happens if apply-generic is called with two arguments of type scheme-number or two arguments of type complex for an operation that is not found in the table for those types? For example, assume that we've defined a generic exponentiation operation:
;
;(define (exp x y) (apply-generic 'exp x y))
;
;and have put a procedure for exponentiation in the Scheme-number package but not in any other package:
;
;;; following added to Scheme-number package
;(put 'exp '(scheme-number scheme-number)
;     (lambda (x y) (tag (expt x y)))) ; using primitive expt
;
;What happens if we call exp with two complex numbers as arguments?

; => answer
; (complex, complex) => (complex, complex) => (complex, complex) => ... 무한루프

;b. Is Louis correct that something had to be done about coercion with arguments of the same type, or does apply-generic work correctly as is?
; => answer
; 같은 것끼리 형변환하는 것 넣어봤자 소용없다.

;c. Modify apply-generic so that it doesn't try coercion if the two arguments have the same type.
; => answer
;  
; apply generic
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (equal? type1 type2)
                  (error "No method for these types"
                    (list op type-tags))
                  (let ((t1->t2 (get-coercion type1 type2))
                        (t2->t1 (get-coercion type2 type1)))
                    (cond (t1->t2
                           (apply-generic op (t1->t2 a1) a2))
                          (t2->t1
                           (apply-generic op a1 (t2->t1 a2)))
                          (else
                           (error "No method for these types"
                                  (list op type-tags)))))))
              (error "No method for these types"
                     (list op type-tags)))))))


;Exercise 2.82.  Show how to generalize apply-generic to handle coercion in the general case of multiple arguments.
; One strategy is to attempt to coerce all the arguments to the type of the first argument, then to the type of the second argument, and so on.
; Give an example of a situation where this strategy (and likewise the two-argument version given above) is not sufficiently general.
; (Hint: Consider the case where there are some suitable mixed-type operations present in the table that will not be tried.)
; =>
; f a b 가 호출되고, f b a 가 등록되어 있는 경우 안되겠네.


(define (install-type-int)
  (put 'add '(int int int) (lambda (x y z) (displayln 'called-add-int))))

(define (install-type-rational)
  (put 'add '(rational rational rational) (lambda (x y z) (displayln 'called-add-rational))))

(define (install-type-real)
  (put 'add '(real real real) (lambda (x y z) (displayln 'called-add-real))))

(define (install-type-complex)
  (put 'add '(complex complex complex) (lambda (x y z) (displayln 'called-add-complex))))

(define (int->rational n)
  (attach-tag 'rational (contents n)))

(define (int->real n)
  (attach-tag 'real (contents n)))

(define (int->complex n)
  (attach-tag 'complex (contents n)))

(define (rational->real n)
  (attach-tag 'real (contents n)))

(define (rational->complex n)
  (attach-tag 'complex (contents n)))

(define (real->complex n)
  (attach-tag 'complex (contents n)))

(put-coercion 'int      'rational int->rational)
(put-coercion 'int      'real     int->real)
(put-coercion 'int      'complex  int->complex)
(put-coercion 'rational 'real     rational->real)
(put-coercion 'rational 'complex  rational->complex)
(put-coercion 'real     'complex  real->complex)




(define (apply-generic2 op . args)
  (define (convert type arg)
    (let ((arg-type (type-tag arg)))
      (let ((proc (get-coercion arg-type type)))
        (if (equal? arg-type type)
            arg
            (if proc
                (proc arg)
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


(install-type-int)
(install-type-rational)
(install-type-real)
;(install-type-complex)

(define (make-int      n) (attach-tag 'int      n))
(define (make-rational n) (attach-tag 'rational n))
(define (make-real     n) (attach-tag 'real     n))
(define (make-complex  n) (attach-tag 'complex  n))

(define (add x y z) (apply-generic2 'add x y z))
(add (make-int 3) (make-int 1) (make-int 1))
(add (make-int 3) (make-rational 1) (make-real 1))
(add (make-int 3) (make-rational 1) (make-complex 1))
; output
;called-add-int
;called-add-real
; No method for these types (add (int rational complex))


;Exercise 2.83.  Suppose you are designing a generic arithmetic system for dealing with the tower of types shown in figure 2.25: integer, rational, real, complex.
; For each type (except complex), design a procedure that raises objects of that type one level in the tower.
; Show how to install a generic raise operation that will work for each type (except complex).
; =>

(define (raise x) (apply-generic2 'raise x))

(define (int->rational n) (make-rational (contents n)))
(define (rational->real n) (make-real (contents n)))
(define (real->complex n)  (make-complex (contents n)))

(put 'raise 'int      int->rational)
(put 'raise 'rational rational->real)
(put 'raise 'real     real->complex)


;Exercise 2.84.  Using the raise operation of exercise 2.83,
; modify the apply-generic procedure so that it coerces its arguments to have the same type by the method of successive raising,
; as discussed in this section.
; You will need to devise a way to test which of two types is higher in the tower.
; Do this in a manner that is ``compatible'' with the rest of the system and will not lead to problems in adding new levels to the tower.



;Exercise 2.85.  This section mentioned a method for ``simplifying'' a data object by lowering it in the tower of types as far as possible.
; Design a procedure drop that accomplishes this for the tower described in exercise 2.83.
; The key is to decide, in some general way, whether an object can be lowered.
; For example, the complex number 1.5 + 0i can be lowered as far as real,
; the complex number 1 + 0i can be lowered as far as integer, and the complex number 2 + 3i cannot be lowered at all.
; Here is a plan for determining whether an object can be lowered: Begin by defining a generic operation project that ``pushes'' an object down in the tower.
; For example, projecting a complex number would involve throwing away the imaginary part.
; Then a number can be dropped if, when we project it and raise the result back to the type we started with, we end up with something equal to what we started with.
; Show how to implement this idea in detail, by writing a drop procedure that drops an object as far as possible.
; You will need to design the various projection operations53 and install project as a generic operation in the system.
; You will also need to make use of a generic equality predicate, such as described in exercise 2.79.
; Finally, use drop to rewrite apply-generic from exercise 2.84 so that it ``simplifies'' its answers.


;Exercise 2.86.  Suppose we want to handle complex numbers whose real parts, imaginary parts, magnitudes, and angles can be either
; ordinary numbers, rational numbers, or other numbers we might wish to add to the system.
; Describe and implement the changes to the system needed to accommodate this.
; You will have to define operations such as sine and cosine that are generic over ordinary numbers and rational numbers.


