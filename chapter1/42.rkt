#lang sicp

(define (inc x) (+ 1 x))
(define (square x) (* x x))

(define (compose fun1 fun2)
  (lambda (x) (fun1 (fun2 x)))
)

; (x + 1)^2
(display ((compose square inc) 6))
(newline)
; x^2 + 1
(display ((compose inc square) 6))