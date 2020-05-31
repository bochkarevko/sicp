#lang sicp

(#%require rackunit)

(define e 2.718281828)
(define tolerance 0.00001)
(define (id x) x)
(define (inc x) (+ x 1))
(define (dec x) (- x 1))
(define (abs-diff x y) (abs (- x y)))
(define (const k) (lambda (i) k))

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

(define (cont-frac n d k)
  (define (next-acc i acc) (/ (n i) (+ (d i) acc)))
  (define (iter i acc)
    (if (= i 0)
      acc
      (iter (dec i) (next-acc i acc))))
  (iter k 0.0))

(define (euler-ap k)
	(+ 2 
		(cont-frac 
			(const 1) 
			(lambda (i) (if (= 2 (remainder i 3)) (* 2.0 (/ (inc i) 3.0)) 1)) 
			k)))

(check-equal? (find-appx euler-ap e) 2.7182)