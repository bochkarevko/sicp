#lang sicp

(#%require rackunit)

(define (sum term a next b)
    (define (iter a acc)
        (if (> a b)
            acc
            (iter (next a) (+ acc (term a)))))
    (iter a 0))