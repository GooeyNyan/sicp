(define tolerance 0.000001)

(define (average a b) (/ (+ a b) 2))

(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display next)
      (if (close-enough? next guess)
        next
        (try next))))
  (try first-guess))

(define (average-dump f)
  (lambda (y) (average y (f y))))

(define (f x)
  (fixed-point 
    (average-dump (lambda (y) (/ (log 1000) (log y))))
    2.0))

(f 100)
