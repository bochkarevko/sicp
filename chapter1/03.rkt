#lang sicp

(#%require rackunit)

(define (solution a b c)
    (if (> a b)
        (if (> b c)
            (+
                (* a a)
                (* b b))
            (+
                (* a a)
                (* c c))
        )
        (if (> a c)
            (+
                (* a a)
                (* b b))
            (+
                (* b b)
                (* c c))
        )
    )
)

(check-equal? (solution 1 2 3) (+ 4 9))
(check-equal? (solution 6 2 1) (+ 36 4))
(check-equal? (solution 6 2 5) (+ 36 25))
