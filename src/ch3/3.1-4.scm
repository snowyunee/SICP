#lang scheme

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))
(W1 50)
50
(W2 70)
30
(W2 40)
"Insufficient funds"
(W1 40)
10

(define acc (make-account 100))
((acc 'withdraw) 50)
((acc 'withdraw) 60)
((acc 'deposit) 40)
((acc 'withdraw) 60)


; Exercise 3.1.  An accumulator is a procedure that is called repeatedly with a single numeric argument and accumulates its arguments into a sum. Each time it is called, it returns the currently accumulated sum. Write a procedure make-accumulator that generates accumulators, each maintaining an independent sum. The input to make-accumulator should specify the initial value of the sum; for example
; 
(define (make-accumulator sum)
  (lambda (amount)
    (begin (set! sum (+ sum amount))
           sum)))
(define A (make-accumulator 5))
(A 10)
15
(A 10)
25



; Exercise 3.2.  In software-testing applications, it is useful to be able to count the number of times a given procedure is called during the course of a computation. Write a procedure make-monitored that takes as input a procedure, f, that itself takes one input. The result returned by make-monitored is a third procedure, say mf, that keeps track of the number of times it has been called by maintaining an internal counter. If the input to mf is the special symbol how-many-calls?, then mf returns the value of the counter. If the input is the special symbol reset-count, then mf resets the counter to zero. For any other input, mf returns the result of calling f on that input and increments the counter. For instance, we could make a monitored version of the sqrt procedure:
; 
(define (make-monitored f)
  (define call-count 0)
  (define (increase-call-count)
    (set! call-count (+ call-count 1)))
  (define (dispatch m)
    (cond ((eq? m 'how-many-calls?) call-count)
          (else 
            (increase-call-count)
            (f m))))
  dispatch)
(define s (make-monitored sqrt))

(s 100)
10

(s 'how-many-calls?)
1



; Exercise 3.3.  Modify the make-account procedure so that it creates password-protected accounts. That is, make-account should take a symbol as an additional argument, as in
;
(define (make-account2 balance pw)
  (define acc (make-account 0))

  (define (incorrect b)
    "Incorrect password")

  (define (dispatch pw2 m)
    (cond ((not (eq? pw pw2))
            (lambda (b) "Incorrect password"))
          (else (acc m))))

  (begin (set! acc (make-account balance))
         dispatch))

(define acc2 (make-account2 100 'secret-password))
; 
; The resulting account object should process a request only if it is accompanied by the password with which the account was created, and should otherwise return a complaint:

((acc2 'secret-password 'withdraw) 40)
60

((acc2 'some-other-password 'deposit) 50)
"Incorrect password"



; Exercise 3.4.  Modify the make-account2 procedure of exercise 3.3 by adding another local state variable so that, if an account is accessed more than seven consecutive times with an incorrect password, it invokes the procedure call-the-cops.

(define (make-account3 balance pw)
  (define acc (make-account2 0 'default))
  (define failcnt 0)
  (define (call-the-cops b) "call-the-cops")

  (define (dispatch pw2 m)
    (cond ((and (eq? failcnt 6) (not (eq? pw pw2))) call-the-cops)
          ((not (eq? pw pw2))
            (begin (set! failcnt (+ failcnt 1))
                   (acc pw2 m)))
          (else (acc pw2 m))))

  (begin (set! acc (make-account2 balance pw))
         dispatch))

(define acc3 (make-account3 100 'secret-password))

((acc3 'secret-password 'withdraw) 40)
60

((acc3 'some-other-password 'deposit) 50)
((acc3 'some-other-password 'deposit) 50)
((acc3 'some-other-password 'deposit) 50)
((acc3 'some-other-password 'deposit) 50)
((acc3 'some-other-password 'deposit) 50)
((acc3 'some-other-password 'deposit) 50)
((acc3 'some-other-password 'deposit) 50)
"call-the-cops"
((acc3 'some-other-password 'deposit) 50)


