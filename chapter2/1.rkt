#lang sicp

(define (negate x) (- 0 x))

(define (gcd x y)
  (cond
    ((negative? x) (gcd (negate x) y))
    ((negative? y) (gcd x (negate y)))
    ((< x y) (gcd y x))
    ((= y 0) 0)
    (else
      (let ((rem (remainder x y)))
        (if (= rem 0)
          y
          (gcd y rem)
        )
      )
    )
  )
)

(define (make-rat n d)
  (let ((g (gcd n d)))
  (let ((nn (/ n g)) (dd (/ d g)))
    (cond
      ((and (negative? n) (negative? d)) (cons (negate nn) (negate dd)))
      ((negative? d) (cons (negate nn) (negate dd)))
      (else (cons nn dd))
    )
  ))
)
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (display-rat x)
  (newline)
  (display (numer x))
  (display " / ")
  (display (denom x))
)

(define (add-rat x y)
  (make-rat
    (+
      (* (numer x) (denom y))
      (* (numer y) (denom x))
    )
    (* (denom x) (denom y))
  )
)

(define (mul-rat x y)
  (make-rat
    (* (numer x) (numer y))
    (* (denom x) (denom y))
  )
)

(define one-half (make-rat 1 2))

(display-rat one-half)
(display-rat (make-rat -1 2))
(display-rat (make-rat 1 -2))
(display-rat (make-rat -1 -2))

(display-rat (mul-rat (make-rat -1 2) (make-rat 1 -2)))

(define one-third (make-rat 1 3))

(display-rat (add-rat one-third one-third))

; -1/3, which is correct!
(display-rat (mul-rat (make-rat 2 -3) (make-rat 1 2)))
