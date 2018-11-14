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

(define (make-center-percent center percentage)
  (let ((percentage-decimal (/ percentage 100.0)))
    (make-interval (* center (- 1 percentage-decimal))
                   (* center (+ 1 percentage-decimal)))))

(define (make-center-percent center percentage)
  (make-center-width center (* center (/ percentage 100.0))))

(define (center i) 
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))
(define (width i) 
  (/ (- (upper-bound i) (lower-bound i)) 2.0))
(define (percent i)
  (* (/ (width i) (center i)) 100))

(define (mul-interval x y)
  (let ((cx*cy (* (center x) (center y)))
        (px+py (/ (+ (percent x) (percent y)) 100.0)))
    (make-interval (* cx*cy (- 1 px+py))
                   (* cx*cy (+ 1 px+py)))))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define a (make-center-percent 100 1))
(define b (make-center-percent 1000 1))

; center 不唯一，因为本身有误差
(center (div-interval a a))
