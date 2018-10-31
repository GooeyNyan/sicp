(define (sum term next a b)
  (if (> a b)
    0
    (+ (term a) (sum term next (next a) b))))

(define (integral f a b dx)
  (* (sum f (lambda (x) (+ x dx)) (+ a (/ dx 2.0)) b) dx))

(define (cube x) (* x x x))

(integral cube 0 1 0.001)
