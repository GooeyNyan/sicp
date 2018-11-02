(define tolerance 0.000001)
(define (average a b) (/ (+ a b) 2))

(define (iterative-improve close-enough? improve)
  (lambda (guess)
    (let ((next (improve guess)))
      (if (close-enough? next guess)
        next
        ((iterative-improve close-enough? improve) next)))))

(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  ((iterative-improve close-enough? f) first-guess))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeat f times)
  (if (= times 0) 
    (lambda (x) x)
    (compose f (repeat f (- times 1)))))

(define (average-dump f)
  (lambda (x) (/ (+ x (f x)) 2)))

(define (fixed-point-of-transform f transform guess)
  (fixed-point (transform f) guess))

(define (power base n)
  (define (iter x times result)
    (cond ((= times 0) result)
          ((even? times) (iter (square x) (/ times 2) result))
          (else (iter x (- times 1) (* x result)))))
  (iter base n 1))

(define (times-of-average-dump n)
  (define (iter times)
    (if (> (power 2 times) n)
      ; - times 1 is just enough
      (- times 1)
      (iter (+ times 1))))
  (iter 0))

(define (nth-roots n x)
  (define f (lambda (y) (/ x (power y (- n 1)))))
  (fixed-point-of-transform f
                            (repeat average-dump
                                    (times-of-average-dump n))
                            1.0))

(define (sqrt x)
  (nth-roots 2 x))

(define (sqrt n)
  (define (improve-it guess)
    (average (/ n guess) guess))
  (define (good-enough? next guess)
    (< (abs (- next guess)) tolerance))
  ((iterative-improve good-enough? improve-it) 1.0))

(sqrt 256)
