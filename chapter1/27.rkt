#lang sicp

(#%require rackunit)

(define (inc x)
  (+ x 1))

(define (sqr x)
  (* x x))

(define (divides? a b)
  (= 0 (remainder b a)))

(define (find-divisor n test-divisor)
  (cond ((> (sqr test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ 2 test-divisor)))))

(define (smallest-divisor n)
  (if (even? n) 
    2
    (find-divisor n 3)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (remainder (sqr (expmod base (/ exp 2) m)) m))
        (else
          (remainder (* base (expmod base (- exp 1) m)) m))))

(define (carmichael-test n)
  (define (iter x)
    (if (= x n)
        (passed x)
        (if (= (expmod x n n) x)
            (iter (inc x))
            (not-passed x))))
    (iter 2))

(define (passed x)
  (display ". Fermat test passed."))

(define (not-passed x)
  (display ".  Fermat test not passed: ")
  (display x))

(define (full-test x)
  (newline)
  (display x)
  (display " - smallest divisor: ")
  (display (smallest-divisor x))
  (carmichael-test x))

(display "Carmichael numbers")
(full-test 561)
(full-test 1025) ;;; not a Carmichael number
(full-test 1105)
(full-test 1729)
(full-test 2465)
(full-test 2821)
(full-test 6601)