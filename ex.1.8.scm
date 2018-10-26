(define (cube-root x)
  (define (iter x guess)
    (if (good-enough? x guess)
      guess
      (iter x (improve-it x guess))))
  (define (good-enough? x guess)
    (< (abs (- (improve-it x guess) guess)) 0.001))
  (define (improve-it x guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (iter x 1.0))

(cube-root 27)
