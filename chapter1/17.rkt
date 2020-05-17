#lang sicp

(#%require rackunit)

(define (halve x)
    (/ x 2))

(define (double x)
    (* x 2))

(define (even? n)
    (= 0 (remainder n 2)))

(define (*-through-sum a b) 
    (cond   ((or (= b 0) (= a 0)) 0)
            ((= a 1) b)
            ((even? b) (*-through-sum (double a) (halve b)) )
            (else (+ a (*-through-sum a (- b 1))))))

(check-equal? (*-through-sum 0 0) (* 0 0))
(check-equal? (*-through-sum 0 1) (* 0 1))
(check-equal? (*-through-sum 1 0) (* 1 0))
(check-equal? (*-through-sum 1 1) (* 1 1))

(check-equal? (*-through-sum 2 2) (* 2 2))
(check-equal? (*-through-sum 7 5) (* 5 7))
(check-equal? (*-through-sum 3 31) (* 3 31))
(check-equal? (*-through-sum 21 17) (* 17 21))