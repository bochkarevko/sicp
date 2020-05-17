#lang sicp

(#%require rackunit)

(define (solution-rec n)
    (if (> 3 n)
        n
        (+ (solution-rec (- n 1)) (solution-rec (- n 2)) (solution-rec (- n 3)))))

(define (solution-iter n)
    (define (iter i f1 f2 f3)
        (if (= i n)
            (+ f1 f2 f3)
            (iter (+ i 1) (+ f1 f2 f3) f1 f2)))
    (if (> 3 n)
        n
        (iter 3 2 1 0)))

(check-equal? (solution-rec 0) 0)
(check-equal? (solution-rec 3) 3)
(check-equal? (solution-rec 8) 68)
(check-equal? (solution-rec 10) 230)

(check-equal? (solution-iter 0) 0)
(check-equal? (solution-iter 3) 3)
(check-equal? (solution-iter 10) 230)

(check-equal? (solution-iter 30) (solution-rec 30))