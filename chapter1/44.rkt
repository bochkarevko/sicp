#lang sicp

(define (inc x) (+ 1 x))
(define (square x) (* x x))

(define (id x) x)
(define (zero? x) (= 0 x))
(define (compose fun1 fun2)
  (lambda (x) (fun1 (fun2 x)))
)
(define (repeated fun n)
  (if (zero? n)
      id
      (lambda (x) ((compose fun (repeated fun (- n 1))) x))
  )
)

(define (smooth fun dx)
  (lambda (x) (/ (+ (fun (- x dx)) (fun x) (fun (+ x dx))) 3))
)

(define (n-fold-smooth fun dx n)
  (let ((dx-smooth (lambda (f) (smooth f dx))))
    ((repeated dx-smooth n) fun)
  )
)

(define (exmpl x) (+ (* x x 3) 7))
(display (exmpl 2))
(newline)
(display ((smooth exmpl 0.1) 2))
(newline)
(display ((n-fold-smooth exmpl 0.1 10) 2))
