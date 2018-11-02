(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (> n 0)
    (compose f (repeated f (- n 1)))
    (lambda (x) x)))

((repeated square 2) 5)
