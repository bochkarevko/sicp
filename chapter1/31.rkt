#lang sicp

(#%require rackunit)

(define (inc x) (+ x 1))
(define (id x) x)
(define (sqr x) (* x x))

(define (product-rec term a next b)
    (if (> a b)
        1
        (* (term a)
           (product-rec term (next a) next b))))

(define (factorial-rec x)
  (product id 1 inc x))

(define (product term a next b)
    (define (iter a acc)
        (if (> a b)
            acc
            (iter (next a) (* acc (term a)))))
    (iter a 1))

(define (factorial x)
  (product id 1 inc x))

(check-equal? (factorial 10) 3628800)
(check-equal? (factorial 10) (factorial-rec 10))

(define (pi-apx n)
  (define (term x)
    (/ (* x
          (inc x)
          4)
       (sqr (inc (* x 2)))))
  (product term 1.0 inc n))

(check-equal? (* 4 (pi-apx 1000)) 3.14159265359)