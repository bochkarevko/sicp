#lang sicp

(#%require rackunit)

(define (sqr x)
    (* x x))

(define (even? n)
    (= 0 (remainder n 2)))

(define (fast-expt-iter b n) 
    (define (iter b n a)
        (cond   ((= n 0) 1)
                ((= n 1) (* a b))
                ((even? n) (iter (sqr b) (/ n 2) a))
                (else (iter b (- n 1) (* a b)))))
    (iter b n 1))

(check-equal? (fast-expt-iter 2 2) 4)
(check-equal? (fast-expt-iter 3 3) 27)
(check-equal? (fast-expt-iter 2 10) 1024)
(check-equal? (fast-expt-iter 13 6) 4826809)