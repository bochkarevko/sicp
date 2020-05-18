#lang sicp

(#%require rackunit)

(define (sqr x)
    (* x x))

(define (even? n)
    (= 0 (remainder n 2)))

(define (fib n)
    (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
    (cond   ((= count 0) b)
            ((even? count)
            (fib-iter a
                      b
                      (+ (sqr q) (sqr p))
                      (+ (sqr q) (* 2 p q))
                      (/ count 2)))
            (else (fib-iter (+ (* b q) (* a q) (* a p))
                            (+ (* b p) (* a q))
                            p
                            q
                            (- count 1)))))

(check-equal? (fib 0) 0)
(check-equal? (fib 1) 1)
(check-equal? (fib 2) 1)

(check-equal? (fib 10) 55)
(check-equal? (fib 20) 6765)