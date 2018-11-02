(define dx 0.00001)

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (> n 0)
    (compose f (repeated f (- n 1)))
    (lambda (x) x)))

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx))) 3)))

(define (repeated-smooth f times)
  ((repeated smooth times) f))

((repeated-smooth square 8) 3.1111101)
