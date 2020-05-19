#lang sicp

(#%require rackunit)

(define (inc x)
  (+ x 1))

(define (sqr x)
  (* x x))

(define (divides? a b)
  (= 0 (remainder b a)))

(define (find-divisor n test-divisor)
  (define (next x)
    (if (= x 2)
        3
        (+ x 2)))
  (cond ((> (sqr test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (newline))

(define (even? n)
    (= 0 (remainder n 2)))

(define (search-for-primes begin end)
(define _begin (if (even? begin) (inc begin) begin))
  (define (iter x)
    (if (> x end)
      0
      ((timed-prime-test x)
       (iter (+ x 2)))))
  (iter _begin))

;;; (search-for-primes 1000000100 1000000250)
(display "without even divisors")
(newline)
(timed-prime-test 1000000181)
(timed-prime-test 1000000207)
(timed-prime-test 1000000223)
(timed-prime-test 1000000241)
