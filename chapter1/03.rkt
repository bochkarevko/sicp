#lang sicp

(#%require rackunit)

(define (sqr x)
    (* x x))

(define (solution a b c)
    (if (> a b)
        (if (> b c)
            (+
                (sqr a)
                (sqr b))
            (+
                (sqr a)
                (sqr c))
        )
        (if (> a c)
            (+
                (sqr a)
                (sqr b))
            (+
                (sqr b)
                (sqr c))
        )
    )
)

(check-equal? (solution 1 2 3) (+ 4 9))
(check-equal? (solution 6 2 1) (+ 36 4))
(check-equal? (solution 6 2 5) (+ 36 25))
