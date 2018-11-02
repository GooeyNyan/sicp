(define (f a b c)
  (cond ((and (< a b) (< a c)) (sum-of-square b c))
        ((and (< b a) (< b c)) (sum-of-square a c))
        (else (sum-of-square a b))))

(define (sum-of-square a b)
  (+ (square a) (square b)))

(f 1 2 3)
(f 2 3 1)
(f 3 2 1)
