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

; > < = | 0 * 3 = 9
; 1 2 | 2 4 = 2 8       | (1l 2l) (1h 2h)
; 1 2 | -2 4 = -2 8       | (1l 2l) (1h 2h)
; 1 2 | -4 -2 = -8 -2       | (1h 2l) (1l 2h)
; 0 2 结果一样

; -1 2 | 2 4 = -4 8       | (1l 2h) (1h 2h)
; -1 3 | -2 4 = -6 12      | 要比较的结果，(1l 2h) or (1h 2l), (1h 2h) or (1l 2l)
; -1 3 | -4 -2 = -12 4     | (1h 2l) (1l 2l)
; -1 0 结果一样

; -3 -1 | 2 4 = -12 -2  | (1l 2h) (1h 2l)
; -3 -1 | -2 4 = -12 2  | (1l 2h) (1h 1l)
; -3 -1 | -2 -4 = 2 12  | (1h 2l) (1l 2h)
; -3 0 结果一样

; ------------------------------------------

; 1 2 | 2 4 = 2 8         | (1l 2l) (1h 2h)
; -3 -1 | 2 4 = -12 -2    | (1l 2h) (1h 2l)
;; -3 -1 | -2 4 = -12 6   | (1l 2h) (1l 2l)
; -1 2 | 2 4 = -4 8       | (1l 2h) (1h 2h)

;; 1 2 | -2 4 = -4 8      | (1h 2l) (1h 2h)
; -1 3 | -4 -2 = -12 4    | (1h 2l) (1l 2l)
; 1 2 | -4 -2 = -8 -2     | (1h 2l) (1l 2h)
;; -3 -1 | -4 -2 = 2 12   | (1h 2h) (1l 2l)

; -1 3 | -2 4 = -6 12     | 要比较的结果，(1l 2h) or (1h 2l), (1h 2h) or (1l 2l)

(define (mul-interval x y)
  (define (span-zero? a b)
    (and (< a 0) (>= b 0)))
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond 
      ; (lower-bound x) > 0 的三种情况
      ((and (>= lx 0) (>= ly 0)) 
       (make-interval (* lx ly) (* ux uy)))
      ((and (>= lx 0) (< uy 0))
       (make-interval (* ux ly) (* lx uy)))
      ((and (>= lx 0) (span-zero? ly uy))
       (make-interval (* ux ly) (* ux uy)))

      ; (upper-bound x) < 0 的三种情况
      ((and (< ux 0) (>= ly 0))
       (make-interval (* lx uy) (* ux ly)))
      ((and (< ux 0) (< uy 0))
       (make-interval (* ux uy) (* lx ly)))
      ((and (< ux 0) (span-zero? ly uy))
       (make-interval (* lx uy) (* lx ly)))

      ; (span-zero? x) 的三种情况
      ((and (span-zero? lx ux) (>= ly 0))
       (make-interval (* lx uy) (* ux uy)))
      ((and (span-zero? lx ux) (< uy 0))
       (make-interval (* ux ly) (* lx ly)))
      ((and (span-zero? lx ux) (span-zero? ly uy))
       (make-interval (min (* lx uy) (* ux ly))
                      (max (* ux uy) (* lx ly)))))))

(define (div-interval x y)
  (define (span-zero? y)
    (and (<= (lower-bound y) 0)
         (>= (upper-bound y) 0)))
  (if (span-zero? y)
    (error "Error: The denominator should not span 0.")
    (mul-interval
      x
      (make-interval (/ 1.0 (upper-bound y))
                     (/ 1.0 (lower-bound y))))))

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

(define (make-center-width center width)
  (make-interval (- center width) (+ center width)))
(define (center i) 
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (interval-width x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))
(define width interval-width)

(define a (make-interval 2 4))
(define b (make-interval -2 4))
(define c (make-interval -4 -2))
(mul-interval a a)
(mul-interval a b)
(mul-interval a c)
(mul-interval b a)
(mul-interval b b)
(mul-interval b c)
(mul-interval c a)
(mul-interval c b)
(mul-interval c c)
