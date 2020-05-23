#lang sicp

(#%require rackunit)

(define (inc x)
  (+ x 1))

(define (sqr x)
  (* x x))

(define (even? n)
  (= 0 (remainder n 2)))

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
        ;;; mr-check function optimizase time insanely: checking 1117 in a commented below way takes 1648, while with mr-check only takes 4 !!!
        ;;;  (if (and (= 1 (remainder (sqr (expmod base (/ exp 2) m)) m)) (not (or (= 1 (expmod base (/ exp 2) m)) (= (- m 1) (expmod base (/ exp 2) m)))))
        ;;;    0
        ;;;    (remainder (sqr (expmod base (/ exp 2) m)) m)))
        (else
          (remainder (* base (expmod base (- exp 1) m)) m))))

(define (mr-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 2)))))

(define (mr-fast-prime? n times)
  (cond ((= times 0) true)
        ((mr-test n) (mr-fast-prime? n (- times 1)))
        (else false)))

(define (start-prime-test n start-time)
  (define (report-prime elapsed-time)
    (display " Prime: ")
    (display elapsed-time))
  (define (report-not-prime elapsed-time)
    (display " Not prime: ")
    (display elapsed-time))
  (if (mr-fast-prime? n 10)
      (report-prime (- (runtime) start-time))
      (report-not-prime (- (runtime) start-time))))

(define (full-test p)
  (newline)
  (display p)
  (display " - smallest divisor: ")
  (display (smallest-divisor p))
  (start-prime-test p (runtime)))

(display "Miller-Rabin test")
(full-test 7) ;;; Prime
(full-test 13) ;;; Prime
(full-test 349) ;;; Prime
(full-test 561) ;;; Carmichael number
(full-test 1025) ;;; not Prime
(full-test 1105) ;;; Carmichael number
(full-test 1117) ;;; Prime
(full-test 1729) ;;; Carmichael number
(full-test 2465) ;;; Carmichael number
(full-test 2821) ;;; Carmichael number
(full-test 6601) ;;; Carmichael number
;;; (full-test 1000000181)
;;; (full-test 1000000207)
;;; (full-test 1000000223)
;;; (full-test 1000000241)