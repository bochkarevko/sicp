#lang sicp

(#%require rackunit)

(define g-ratio 0.61803398875)
(define tolerance 0.00001)
(define (inc x) (+ x 1))
(define (dec x) (- x 1))
(define (abs-diff x y) (abs (- x y)))

(define (cont-frac n d k)
  (define (next-acc i acc) (/ (n i) (+ (d i) acc)))
  (define (iter i acc)
    (if (= i 0)
      acc
      (iter (dec i) (next-acc i acc))))
  (iter k 0.0))

(define (cont-frac-rec n d k)
  (define (iter i)
    (if (= i k)
      (/ (n i) (d i))
      (/ (n i)
        (+ (d i) (iter (inc i))))))
  (iter 1))

(define (found i value)
  (newline)
  (display "Steps taken: ")
  (display i)
  (newline)
  value)

(define (find-appx f value)
  (define (iter i)
    (let ((curr (f i)))
      (if (< (abs-diff curr value) tolerance)
        (found i curr)
        (iter (inc i)))))
    (iter 1))

(check-equal? (find-appx (lambda (k) (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k)) g-ratio) 0.6180)