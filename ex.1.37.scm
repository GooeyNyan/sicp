(define tolerance 0.000001)

(define (average a b) (/ (+ a b) 2))

(define (fixed-point f first-guess)
  (define (try guess)
    ((let ((next (f guess)))
       (if (close-enough? next guess)
         next
         (try next)))))
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (try first-guess))

(define (average-dump f) 
  (lambda (y) (average y (f y))))

; (define (cont-frac n d k)
;   (define (helper n d a)
;     (if (> a k)
;       0
;       (/ (n a) (+ (d a) (helper n d (+ a 1))))))
;   (helper n d 1))

(define (cont-frac n d k)
  (define (iter a result)
    (if (= a 0)
      result
      (iter (- a 1) (/ (n a) (+ (d a) result)))))
  (iter k 0))

(/ 1 (cont-frac (lambda (i) 1.0)
                (lambda (i) 1.0)
                100))
