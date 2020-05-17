#lang sicp

(#%require rackunit)

(define (out-of-bounds? row col)
    (or (> col row) (> 0 row) (> 0 col)))

(define (edge-one? row col)
    (or (= 0 col) (= row col)))

(define (pascal row col)
    (cond   ((out-of-bounds? row col) 0)
            ((edge-one? row col) 1)
            (else (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col)))))

(check-equal? (pascal 0 0) 1)
(check-equal? (pascal 4 2) 6)
(check-equal? (pascal 7 3) 35)
(check-equal? (pascal 10 1) 10)
(check-equal? (pascal 10 5) 252)