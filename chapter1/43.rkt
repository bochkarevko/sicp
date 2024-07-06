#lang sicp

(define (inc x) (+ 1 x))
(define (square x) (* x x))

(define (id x) x)
(define (zero? x) (= 0 x))
(define (even? x) (= 0 (remainder x 2)))

(define (repeated-naive fun n)
  (if (zero? n)
    id
    (lambda (x) (fun ((repeated-naive fun (- n 1)) x)))
  )
)

(define (compose fun1 fun2)
  (lambda (x) (fun1 (fun2 x)))
)
(define (repeated-compose fun n)
  (if (zero? n)
      id
      (lambda (x) ((compose fun (repeated-compose fun (- n 1))) x))
  )
)

(define (repeated fun n)
  (cond
    ((zero? n) id)
    ((even? n)
      (let ((twofold (repeated fun (/ n 2))))
        (lambda (x) (twofold (twofold x)))
      )
    )
    (else (lambda (x) ((compose fun (repeated fun (- n 1))) x)))
  )
)

(display ((repeated-naive square 2) 5))
(newline)
(display ((repeated-compose square 2) 5))
(newline)
(display ((repeated square 2) 5))
(newline)

(#%require rackunit)

(check-equal? ((repeated square 3) 6) ((repeated-compose square 3) 6))
(check-equal? ((repeated square 3) 6) ((repeated-naive square 3) 6))

(check-equal? ((repeated inc 4) 4) ((repeated-compose inc 4) 4))
(check-equal? ((repeated inc 4) 4) ((repeated-naive inc 4) 4))

(check-equal? ((repeated (compose square inc) 5) 2) ((repeated-compose (compose square inc) 5) 2))
(check-equal? ((repeated (compose square inc) 5) 2) ((repeated-naive (compose square inc) 5) 2))
