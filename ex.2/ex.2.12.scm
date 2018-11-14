(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (make-center-percent center percentage)
  (let ((percentage-decimal (/ percentage 100.0)))
    (make-interval (* center (- 1 percentage-decimal))
                   (* center (+ 1 percentage-decimal)))))
(define (center i) 
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))
(define (width i) 
  (/ (- (upper-bound i) (lower-bound i)) 2.0))
(define (percent i)
  (* (/ (width i) (center i)) 100))

(define d (make-center-percent 10 10))
(percent d)
(width d)