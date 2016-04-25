#lang scheme

(require "3.1-4.scm")

; Exercise 3.7.  Consider the bank account objects created by make-account, with the password modification described in exercise 3.3. Suppose that our banking system requires the ability to make joint accounts. Define a procedure make-joint that accomplishes this. Make-joint should take three arguments. The first is a password-protected account. The second argument must match the password with which the account was defined in order for the make-joint operation to proceed. The third argument is a new password. Make-joint is to create an additional access to the original account using the new password. For example, if peter-acc is a bank account with password open-sesame, then

;(define paul-acc
;    (make-joint peter-acc 'open-sesame 'rosebud))

;will allow one to make transactions on peter-acc using the name paul-acc and the password rosebud. You may wish to modify your solution to exercise 3.3 to accommodate this new feature.

(define (make-joint account origin-pw my-pw)
  (lambda (pw msg)
    (if (not (eq? my-pw pw)) (lambda (b) "Incorrect password")
      (account origin-pw msg))))

(define peter-acc (make-account2 100 'secret-password))
((peter-acc 'secret-password 'withdraw) 40)
60

(define paul-acc
    (make-joint peter-acc 'secret-password 'rosebud))
((paul-acc 'roebud 'withdraw) 10)

((paul-acc 'rosebud 'withdraw) 10)
50

(define paul-acc-wrong-pw
    (make-joint peter-acc 'secret-pw 'rosebud))
((paul-acc-wrong-pw 'rosebud 'withdraw) 10)
"Incorrect password"


;Exercise 3.8.  When we defined the evaluation model in section 1.1.3, we said that the first step in evaluating an expression is to evaluate its subexpressions. But we never specified the order in which the subexpressions should be evaluated (e.g., left to right or right to left). When we introduce assignment, the order in which the arguments to a procedure are evaluated can make a difference to the result. Define a simple procedure f such that evaluating (+ (f 0) (f 1)) will return 0 if the arguments to + are evaluated from left to right but will return 1 if the arguments are evaluated from right to left.


(define f 
  (let ((s 0))
    (define (update ns)
      (let ((r s))
        (set! s ns)
        r))
    update))

;(f 0)
;'->0
;(f 1)
;'->0
;
;-------------

(f 1)
'->0
(f 0)
'->1
