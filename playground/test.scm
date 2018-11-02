(define tolerance 0.0000000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(fixed-point cos 1.0)

(define (average a b) (/ (+ a b) 2))

(define (sqrt x)
  (fixed-point 
    (lambda (y) (average y (/ x y)))
    1.0))

(sqrt 9.0)

(define dx 0.00001)

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (cube x) (* x x x))

((deriv cube) 5)

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (newtons-method
    (lambda (y) (- (square y) x)) 1.0))

(define (average-dump f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point-of-transform
    (lambda (y) (/ x y)) average-dump 1.0))

(define (sqrt x)
  (fixed-point-of-transform
    (lambda (y) (- (square y) x)) newton-transform 1.0))
