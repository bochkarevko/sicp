#lang sicp

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (found i value)
  (newline)
  (display "Steps taken: ")
  (display i)
  (newline)
  value)

; modified from previous tasks
(define (find-appx f check)
  (define (iter i)
    (let ((curr (f i)))
      (if (check curr)
        (found i curr)
        (iter (inc i)))))
    (iter 1))

(define (cont-frac numerator-fun denominator-fun steps)
  (define (next-acc step acc) (/ (numerator-fun step) (+ (denominator-fun step) acc)))
  (define (iter i acc)
    (if (= i 0)
      acc
      (iter (dec i) (next-acc i acc))))
  (iter steps 0.0))

(define (tan-cf x k)
  (cont-frac
    (lambda (step) (if (= step 1) x (- 0 (* x x))))
    (lambda (step) (- (* 2 step) 1))
    k
  )
)

; tests
(define tolerance 0.00001)
(define (neg-dec-4 x) (/ (ceiling (* 10000 x)) 10000))
(define (abs-diff x y) (abs (- x y)))
(define (checker target guess) (< (abs-diff (neg-dec-4 guess) target) tolerance))

(find-appx (lambda (i) (tan-cf 3 i)) (lambda (guess) (checker -0.1425 guess)))
(find-appx (lambda (i) (tan-cf 666 i)) (lambda (guess) (checker -0.0176 guess)))
(find-appx (lambda (i) (tan-cf 23 i)) (lambda (guess) (checker 1.5882 guess)))