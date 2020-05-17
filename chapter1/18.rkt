#lang sicp

(#%require rackunit)

(define (halve x)
    (/ x 2))

(define (double x)
    (* x 2))

(define (even? n)
    (= 0 (remainder n 2)))

(define (*-through-sum-iter a b) 
    (define (iter a b acc)
        (cond   ((= b 1) (+ a acc))
                ((even? b) (iter (double a) (halve b) acc))
                (else (iter a (- b 1) (+ acc a)))))
    (if (or (= 0 a) (= 0 b))
        0
        (iter a b 0)))

(check-equal? (*-through-sum-iter 0 0) (* 0 0))
(check-equal? (*-through-sum-iter 0 1) (* 0 1))
(check-equal? (*-through-sum-iter 1 0) (* 1 0))
(check-equal? (*-through-sum-iter 1 1) (* 1 1))

(check-equal? (*-through-sum-iter 2 2) (* 2 2))
(check-equal? (*-through-sum-iter 7 5) (* 5 7))
(check-equal? (*-through-sum-iter 3 31) (* 3 31))
(check-equal? (*-through-sum-iter 21 17) (* 17 21))