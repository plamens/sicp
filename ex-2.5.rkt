#lang sicp

(#%require rackunit)


(define (consx a b)
  (* (expt 2 a) (expt 3 b)))

(define (carx x)
  (if (eq? (remainder x 2 ) 0)
      (+ 1 (carx (/ x 2)))
      0))

(define (cdrx x)
  (if (eq? (remainder x 3) 0)
      (+ 1 (cdrx (/ x 3)))
      0))


(define x (consx 5 6))
(check-equal? (carx x) 5)
(check-equal? (cdrx x) 6)