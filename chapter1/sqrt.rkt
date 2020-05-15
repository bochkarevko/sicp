#lang sicp

(#%require rackunit)

(define (sqr x)
    (* x x))

(define (abs x)
    (if (< x 0)
        (- x)
        x))

(define (avg x y)
    (/  (+ x y)
        2))

(define (improve-guess guess x)
    (avg guess (/ x guess)))

(define (good-enough? guess x) 
    (> 0.001
        (abs (- x
                (sqr guess)))))

(define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve-guess guess x) x)))

(define (sqrt x)
    (sqrt-iter 1.0 x))

(check-equal? (sqrt 0.0) 0.0)
(check-equal? (sqrt 1.0) 1.0)
(check-equal? (sqrt 778) 27.892651362)
(check-equal? (sqrt 666) 25.8069758011)