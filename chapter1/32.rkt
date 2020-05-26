#lang sicp

(#%require rackunit)

(define (accumulate-rec combiner null-value term a next b)
  (define (iter x)
    (if (> x b)
      null-value
      (combiner 
        (term x)
        (iter (next x)))))
  (iter a))

(define (accumulate combiner null-value term a next b)
  (define (iter x acc)
    (if (> x b)
      acc
      (iter 
        (next x)
        (combiner 
          (term x)
          acc))))
  (iter a null-value))

(define (inc x) (+ x 1))
(define (id x) x)
(define (sqr x) (* x x))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
    (accumulate * 1 term a next b))

(define (factorial x)
  (product id 1 inc x))

(check-equal? (sum id 1 inc 10) 55)
(check-equal? (factorial 9) 362880)
(check-equal? (factorial 10) 3628800)