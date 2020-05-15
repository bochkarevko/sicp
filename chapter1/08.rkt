#lang sicp

(#%require rackunit)

(define (sqr x)
    (* x x))

(define (cub x)
    (* x x x))

(define (abs x)
    (if (< x 0)
        (- x)
        x))

(define (avg x y)
    (/  (+ x y)
        2))

(define (cubrt x)

    (define (good-enough? guess)
        (>  0.001
            (/ (abs (- x (cub guess)))
                (abs x))))

    (define (improve-guess guess)
        (/  (+  (/  x
                    (sqr guess))
                (* 2 guess))
            3))

    (define (cubrt-iter guess)
        (if (good-enough? guess)
            guess
            (cubrt-iter (improve-guess guess))))

    (if (= x 0) 0.0
        (cubrt-iter 1.0)))

(check-equal? (cubrt 0.0) 0.0)
(check-equal? (cubrt 0.000027) 0.03)
(check-equal? (cubrt 0.008) 0.2)
(check-equal? (cubrt 1.0) 1.0)
(check-equal? (cubrt 2.0) 1.25992104989)
(check-equal? (cubrt 666) 8.7328917413)
(check-equal? (cubrt 778) 9.19728968681)
(check-equal? (cubrt 7776665) 198.121282359)