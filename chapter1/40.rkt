#lang sicp

(define tolerance 0.0001)

(define (fixed-point fun first-guess)
  (define (close-enough? val1 val2)
    (<
      (abs (- val1 val2))
      tolerance
    )
  )
  (define (try guess)
    (let ((next-guess (fun guess)))
      (if (close-enough? guess next-guess)
        next-guess
        (try next-guess)
      )
    )
  )
  (try first-guess)
)

(define (average-damp fun)
  (lambda (x) (/ (+ x (fun x)) 2))
)

(define (sq-root x)
  (fixed-point
    (average-damp (lambda (y) (/ x y)))
    1.0
  )
)

(define dx tolerance)
(define (derivative fun)
  (lambda (x)
    (/
      (- (fun (+ x dx)) (fun x))
      dx
    )
  )
)

(define (newton-transform fun)
  (lambda (x)
    (- x (/ (fun x) ((derivative fun) x)))
  )
)

(define (newtons-method fun first-guess)
  (fixed-point (newton-transform fun) first-guess)
)

(define (newton-sqrt x)
  (newtons-method (lambda (y) (- (* y y) x)) 1.0)
)

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c))
)

(display (newtons-method (cubic -3 1 7) 1)) ; -1.18 VS -1.179...