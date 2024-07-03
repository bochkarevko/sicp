#lang sicp

; f : (Int -> T) -> T
(define (f g) (g 2))

; lambda (x) (* z (+ z 1)) : Int -> Int
(f (lambda (x) (* z (+ z 1))))

; error: expected a procedure
(f f)