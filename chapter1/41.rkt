#lang sicp

(define (inc x) (+ 1 x))

(define (double fun)
  (lambda (x) (fun (fun x)))
)

; inc (inc 5)
(display ((double inc) 5))
(newline)
; double double -> lambda (fun) double (double fun) -inc-> double (inc inc) -> inc x4
(display (((double double) inc) 5))
(newline)
; prev twice -> inc x 8
(display ((double (double (double inc))) 5))
(newline)
; double double -> lambda (fun) double (double fun)
; double dd -> double (double (double (double fun))) -inc-> x2 x2 x2 x2 inc -> inc x16
(display (((double (double double)) inc) 5))