#lang scheme


;(define rand
;  (let ((x random-init))
;    (lambda ()
;      (set! x (rand-update x))
;      x)))

;===============================================================

;(define (estimate-pi trials)
;  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

;(define (cesaro-test)
;  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;===============================================================

;(define (estimate-pi2 trials)
;  (sqrt (/ 6 (random-gcd-test trials random-init))))

;(define (random-gcd-test trials initial-x)
;  (define (iter trials-remaining trials-passed x)
;    (let ((x1 (rand-update x)))
;      (let ((x2 (rand-update x1)))
;        (cond ((= trials-remaining 0)   
;               (/ trials-passed trials))
;              ((= (gcd x1 x2) 1)
;               (iter (- trials-remaining 1)
;                     (+ trials-passed 1)
;                     x2))
;              (else
;                (iter (- trials-remaining 1)
;                      trials-passed
;                      x2))))))
;  (iter trials 0 initial-x))

;===============================================================

;Exercise 3.5.  Monte Carlo integration is a method of estimating definite integrals by means of Monte Carlo simulation. Consider computing the area of a region of space described by a predicate P(x, y) that is true for points (x, y) in the region and false for points not in the region. For example, the region contained within a circle of radius 3 centered at (5, 7) is described by the predicate that tests whether (x - 5)2 + (y - 7)2< 32. To estimate the area of the region described by such a predicate, begin by choosing a rectangle that contains the region. For example, a rectangle with diagonally opposite corners at (2, 4) and (8, 10) contains the circle above. The desired integral is the area of that portion of the rectangle that lies in the region. We can estimate the integral by picking, at random, points (x,y) that lie in the rectangle, and testing P(x, y) for each point to determine whether the point lies in the region. If we try this with many points, then the fraction of points that fall in the region should give an estimate of the proportion of the rectangle that lies in the region. Hence, multiplying this fraction by the area of the entire rectangle should produce an estimate of the integral.
;
;Implement Monte Carlo integration as a procedure estimate-integral that takes as arguments a predicate P, upper and lower bounds x1, x2, y1, and y2 for the rectangle, and the number of trials to perform in order to produce the estimate. Your procedure should use the same monte-carlo procedure that was used above to estimate . Use your estimate-integral to produce an estimate of  by measuring the area of a unit circle.
;
;You will find it useful to have a procedure that returns a number chosen at random from a given range. The following random-in-range procedure implements this in terms of the random procedure used in section 1.2.6, which returns a nonnegative number less than its input.8

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (* (* (- x2 x1) (- y2 y1)) (monte-carlo trials (lambda () (p x1 x2 y1 y2)))))

(define (integral-test x1 x2 y1 y2)
  (let* ((x (random-in-range x1 x2))
         (y (random-in-range y1 y2)))
    (lambda ()
      (<= (+ (expt (- x 5) 2) (expt (- y 7) 2))
          (expt 3 2)))))

;test
(estimate-integral integral-test 2 8 4 10 1000)
;36

;Exercise 3.6.  It is useful to be able to reset a random-number generator to produce a sequence starting from a given value. Design a new rand procedure that is called with an argument that is either the symbol generate or the symbol reset and behaves as follows: (rand 'generate) produces a new random number; ((rand 'reset) <new-value>) resets the internal state variable to the designated <new-value>. Thus, by resetting the state, one can generate repeatable sequences. These are very handy to have when testing and debugging programs that use random numbers.

(define (rand-update x)
  (let ((a (expt 2 32))
        (c 1103515245)
        (m 12345))
    (modulo (+ (* a x) c) m)))

(define random-init 137)

(define rand 
  (let ((x random-init))
    (define (dispatch m)
      (cond ((eq? m 'generate)
             (begin (set! x (rand-update x))
                    x))
            ((eq? m 'reset) 
             (lambda (new-x)
               (set! x new-x)))
            (else (error "unknown request"))))
    dispatch))

; test
(rand 'generate)
;Value: 3062
(rand 'generate) 
;Value: 1397
((rand 'reset) 3062)
(rand 'generate)
;Value: 1397

