#lang scheme

; get / put
(define global-array `())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list (make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))




; e 2.74
(define (apply-generic op file . args)
  (let ((proc (get op (type-tag file))))
    (if proc
      (proc (contents file) args)
      (error "no proc"))))

(define (type-tag file)
  (car file))

(define (contents file)
  (cdr file))


(define (install-division-a-package)
  (define (get-record file args)
    (let ((name (car args)))
      (cond ((null? file) (error "null file"))
            ((eqv? name (get-name (cdar file) `()) ) (car file))
            (else (get-record (cdr file) args)))))
  (define (get-name row args)
    (car row))
  (define (get-salary row args)
    (cadr row))
  (put 'get-record 'division-a get-record)
  (put 'get-salary 'division-a get-salary))

(define (install-division-b-package)
  (define (get-record file args)
    (let ((name (car args)))
      (cond ((null? file) (error "null file"))
            ((eqv? name (get-name (cdar file) `())) (car file))
            (else (get-record (cdr file) args)))))
  (define (get-name row args)
    (car row))
  (define (get-salary row args)
    (caddr row))
  (put 'get-record 'division-b get-record)
  (put 'get-salary   'division-b get-salary))

  (define (make-row-division-a name salary phone)
    (cons `division-a (list name salary phone)))

  (define (make-row-division-b name salary phone)
    (cons `division-b (list name phone salary)))

(define (get-record file name)        (apply-generic `get-record file name))
(define (get-salary row)              (apply-generic `get-salary row))

(define (division-a-file)
  (cons 'division-a
        (list (make-row-division-a `name1 100 `010-111-1111)
              (make-row-division-a `name2 200 `020-111-1111)
              (make-row-division-a `name3 300 `030-111-1111))))

(define (division-b-file)
  (cons 'division-b
        (list (make-row-division-b `name1 100 `010-111-1111)
              (make-row-division-b `name2 200 `020-111-1111)
              (make-row-division-b `name3 300 `030-111-1111))))

(install-division-a-package)
(install-division-b-package)


(get-record (division-a-file) `name1)
(get-salary (get-record (division-a-file) `name1))
(get-record (division-b-file) `name1)
(get-salary (get-record (division-b-file) `name1))
;(division-a name1 100 010-111-1111)
;100
;(division-b name1 010-111-1111 100)
;100
 
