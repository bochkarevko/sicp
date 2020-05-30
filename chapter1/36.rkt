#lang sicp

(#%require rackunit)

(define tolerance 0.00001)
(define (close-enough? x y) (< (abs (- x y)) tolerance))
(define (avg x y) (/ (+ x y) 2.0))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ( (next (f guess)) )
      (newline)
      (display "Current guess: ")
      (display guess)
      (if (close-enough? next guess)
        guess
        (try (avg next guess))))) 
  (try first-guess))

(define (x-pow-x-eq y) (fixed-point (lambda (x) (/ (log y) (log x))) 2.7))
(define solution (x-pow-x-eq 1000))
;;; without avg it takes 33 tries, with avg it takes 9 tries
(check-equal? solution 4.5555)