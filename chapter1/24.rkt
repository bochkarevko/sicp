#lang sicp

(#%require rackunit)

(define (inc x)
  (+ x 1))

(define (sqr x)
  (* x x))

(define (even? n)
  (= 0 (remainder n 2)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (remainder (sqr (expmod base (/ exp 2) m)) m))
        (else
          (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (newline))

(display "Fermat test")
(newline)
(timed-prime-test 1000000181)
(timed-prime-test 1000000207)
(timed-prime-test 1000000223)
(timed-prime-test 1000000241)
