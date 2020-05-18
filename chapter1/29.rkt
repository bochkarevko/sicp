#lang sicp

(#%require rackunit)

(define (even? n)
    (= 0 (remainder n 2)))

(define (inc x)
    (+ x 1))

(define (cube x)
    (* x x x))

(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a)
           (sum term (next a) next b))))

(define (integral f a b dx)
    (define (add-dx x) (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) add-dx b)
       dx))

(define (integral-simp f a b n)
    (define h (/ (- b a) n))
    (define (simp k) (f (+ a (* k h))))
    (define (term k)
        (cond ((= k 0) (f a))
            ((= k n) (f b))
            ((even? k) (* 2
                          (simp k)))
            (else (* 4
                     (simp k)))))
    (/ (* h (sum term 0.0 inc n)) 3))

(check-equal? (integral cube 0 1 0.001) (integral-simp cube 0.0 1.0 100.0))
(check-equal? (integral cube 0 1 0.001) (integral-simp cube 0.0 1.0 1000.0))