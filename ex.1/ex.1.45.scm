(define tolerance 0.000001)

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? next guess)
        next
        (try next))))
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (try first-guess))

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

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-dump
                            1.0))

(define (cube-root x)
  (fixed-point-of-transform (lambda (y) (/ x (square y)))
                            average-dump
                            1.0))

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

(define (cube-roots x)
  (nth-roots 3 x))

(define (fourth-roots x)
  (nth-roots 4 x))

(sqrt 4)

(cube-root 27)

(fourth-roots 16)

; 2 power times >= n
; 2 2^1
; 4 2^2
; 8 2^3
; 10 2^3
; 16 2^4

(times-of-average-dump 16)

(nth-roots 4 16)
(nth-roots 5 32)
(nth-roots 6 64)
(nth-roots 7 128)
(nth-roots 8 256)
(nth-roots 15 (power 2 15))
(nth-roots 16 (power 2 16))
; (nth-roots 256 (power 3 256))
(define a 400)
(nth-roots a (power 3 a))
