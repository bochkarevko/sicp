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

(define (sqrt x)

    (define (good-enough? guess) 
        (> 0.001
            (abs (- x
                    (sqr guess)))))

    (define (improve-guess guess)
        (avg guess (/ x guess)))

    (define (sqrt-iter guess)
        (if (good-enough? guess)
            guess
            (sqrt-iter (improve-guess guess))))

    (sqrt-iter 1.0))

(check-equal? (sqrt 0.0) 0.0)
(check-equal? (sqrt 0.0009) 0.03)
(check-equal? (sqrt 1.0) 1.0)
(check-equal? (sqrt 2.0) 1.41421356237)
(check-equal? (sqrt 778) 27.892651362)
(check-equal? (sqrt 666) 25.8069758011)
(check-equal? (sqrt 7776665) 2788.6672444)