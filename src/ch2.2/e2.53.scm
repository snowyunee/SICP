#lang scheme

; e 2.53
(list `a `b `c)
(list (list `george))
`((x1 x2) (y1 y2))
(cdr `((x1 x2) (y1 y2)))
(cadr `((x1 x2) (y1 y2)))
(pair? (car `(a shrot list)))
(memq `red `((red shoes) (blue socks)))
(memq `red `(red shoes ble socks))
; (a b c)
; ((george))
; ((y1 y2))
; (y1 y2)
; #f
; #f
; (red shoes ble socks)


; 2.54
(define (equal? a b)
  (if (pair? a)
      (if (pair? b)
          (if (eq? (car a) (car b))
              (equal? (cdr a) (cdr b))
              #f)
          #f)
      (if (pair? b)
          #f
          (eq? a b))))
;true
(equal? `(this is a list) `(this is a list))
;false
(equal? `(this is a list) `(this (is a) list))

; 2.55
(car ''abracadabra)
;quote