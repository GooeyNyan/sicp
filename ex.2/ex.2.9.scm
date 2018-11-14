(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
    x
    (make-interval (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (print-interval x)
  (newline)
  (display "(")
  (display (lower-bound x))
  (display " . ")
  (display (upper-bound x))
  (display ")"))

(print-interval 
  (add-interval (make-interval 1 2)
                (make-interval 2 4)))

(define (interval-width x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))

(interval-width (add-interval (make-interval 1 2)
                              (make-interval 2 4)))
(interval-width (make-interval 1 2))
(interval-width (make-interval 2 4))

; + 扩大 width
; - 缩小 width
(define (add-interval-width x y)
  (+ (interval-width x) (interval-width y)))
(define (sub-interval-width x y)
  (- (interval-width x) (interval-width y)))

; * / 不定

; (add-interval-width (make-interval 1 2)
;                     (make-interval 2 4))
; (sub-interval-width (make-interval 2 4)
;                     (make-interval 1 2))
(mul-interval (make-interval 2 4)
              (make-interval 1 2))
(interval-width 
  (mul-interval (make-interval 2 4)
                (make-interval 1 2)))
