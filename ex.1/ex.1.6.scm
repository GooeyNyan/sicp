; new-if is a precedure,
; it eval all parameters
; it will occur a infinity loop
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter n guess)
  (if (good-enough? n guess)
  ; (new-if (good-enough? n guess)
    guess
    (sqrt-iter n (improve-it n guess))))

(define (improve-it n guess)
  (average (/ n guess) guess))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? n guess)
  (< (abs (- (square guess) n)) 0.001))

(define (sqrt n)
  (sqrt-iter n 1))

(sqrt 2)
