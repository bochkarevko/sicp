#lang sicp

(#%require rackunit)

(define tolerance 0.00001)
(define (close-enough? x y) (< (abs (- x y)) tolerance))
(define (avg x y) (/ (+ x y) 2.0))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ( (next (f guess)) )
      (if (close-enough? next guess)
        guess
        (try next))))
  (try first-guess))

(define g-ratio (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

(check-equal? g-ratio 1.61803398875)