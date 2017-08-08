#lang sicp

(#%require rackunit)

(define (make-interval a b) (cons a b))

(define lower-bound car)

(define upper-bound cdr)

(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval 
                 (/ 1.0 (upper-bound y)) 
                 (/ 1.0 (lower-bound y)))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) 
        (upper-bound i)) 
     2))

(define (width i)
  (/ (- (upper-bound i) 
        (lower-bound i)) 
     2))

(define (make-center-percent c p)
  (make-center-width c (abs (* c p))))

(define (percent x)
  (abs (/ (width x) (center x))))

(define (par1 r1 r2)
  (div-interval 
   (mul-interval r1 r2)
   (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval 
     one
     (add-interval 
      (div-interval one r1) 
      (div-interval one r2)))))

(define a (make-interval 5 6))
(define b (make-interval 1 2))
(define a-times-b (mul-interval a b))
(check-equal? (lower-bound a-times-b) 5)
(check-equal? (upper-bound a-times-b) 12)

(define a-over-b (div-interval a b))

(define a-minus-b (sub-interval a b))

(check eq? (lower-bound a-minus-b) 3)
(check eq? (upper-bound a-minus-b) 5)

(define c (make-center-percent 10 0.1))
(check-= (percent c) 0.1 0.001)
(check-= (center c) 10.0 0.001)
(check-= (width c) 1.0 0.001)

(define r1 (par1 a b))
(define r2 (par2 a b))

(display r1)
(display r2)
(display (mul-interval (div-interval r2 (add-interval a b)) (add-interval a b)))

