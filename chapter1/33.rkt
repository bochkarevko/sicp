#lang sicp

(#%require rackunit)

(define (id x) x)
(define (inc x) (+ x 1))
(define (sqr x) (* x x))
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))
(define (expmod base exp m)
  (define (mr-check val)
    (define (main x)
      (if (= 1 x)
        (if (or (= 1 val) (= (- m 1) val))
          x
          0)
        x))
    (main (remainder (sqr val) m)))
  (cond ((= exp 0) 1)
        ((even? exp) (mr-check (expmod base (/ exp 2) m)))
        (else
          (remainder (* base (expmod base (- exp 1) m)) m))))
(define (mr-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))
(define (mr-fast-prime? n times)
  (cond ((= times 0) true)
        ((mr-test n) (mr-fast-prime? n (- times 1)))
        (else false)))
(define (prime? n)
  (if (= n 1)
    true
    (mr-fast-prime? n 5)))

(define (filtered-accumulate combiner filter null-value term a next b)
  (define (iter x acc)
    (if (> x b)
      acc
      (iter 
        (next x)
        (if (filter x)
          (combiner acc (term x))
          acc))))
  (iter a null-value))

(define (prime-sqrs-sum a b)
  (filtered-accumulate + prime? 0 sqr a inc b))

(check-equal? (prime-sqrs-sum 1 6) 39)
(check-equal? (prime-sqrs-sum 1 15) 378)
(check-equal? (prime-sqrs-sum 1 20) (+ 378 (sqr 17) (sqr 19)))

(define (mut-prime-prod n)
  (define (mut-prime? x) (= 1 (gcd x n)))
  (filtered-accumulate * mut-prime? 1 id 1 inc (- n 1)))

(check-equal? (mut-prime-prod 5) (* 1 2 3 4))
(check-equal? (mut-prime-prod 6) (* 1 5))
(check-equal? (mut-prime-prod 9) (* 1 2 4 5 7 8))