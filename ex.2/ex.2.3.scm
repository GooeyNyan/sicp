(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1: CONS" m))))
  dispatch)
(define (car z) (z 0))
(define (cdr z) (z 1))

(define (make-segment p1 p2)
  (cons p1 p2))
(define start-segment car)
(define end-segment cdr)
(define (midpoint-segment s)
  (let ((start (start-segment s))
        (end (end-segment s)))
    (make-point (/ (+ (x-point start)
                      (x-point end)) 2.0)
                (/ (+ (y-point start)
                      (y-point end)) 2.0))))

(define (make-point x y)
  (cons x y))
(define x-point car)
(define y-point cdr)
(define (dist-of-point p1 p2)
  (sqrt (+ (square (- (x-point p1) (x-point p2)))
           (square (- (y-point p1) (y-point p2))))))

; make-rect

; rect-width
; rect-height

; rect-area
; rect-perimeter

; APPLICATION LEVEL
(define (rect-area rect)
  (* (rect-width rect)
     (rect-height rect)))
(define (rect-perimeter rect)
  (* (+ (rect-width rect)
        (rect-height rect))
     2))

(define (rect-width rect)
  (abs (- (x-point (top-left rect))
          (x-point (bottom-right rect)))))
(define (rect-height rect)
  (abs (- (y-point (top-left rect))
          (y-point (bottom-rectight rect)))))

; parameters
; top-left point
; bottom-right point
(define (make-rect top-left bottom-right)
  (cons top-left bottom-right))
(define (top-left rect)
  (car rect))
(define (bottom-right rect)
  (cdr rect))

(rect-area (make-rect (make-point 0 5)
                      (make-point 5 0)))
(rect-perimeter (make-rect (make-point 0 5)
                           (make-point 5 0)))

; parameters
; side segment
; parallel-side segment
(define (make-rect side parallel-side)
  (cons side parallel-side))
(define (side1 rect)
  (car rect))
(define (side2 rect)
  (cdr rect))

(define (side-lengths rect)
  (cons (dist-of-point (start-segment (side1 rect))
                       (end-segment (side1 rect)))
        (min (dist-of-point (start-segment (side1 rect))
                            (start-segment (side2 rect)))
             (dist-of-point (start-segment (side1 rect))
                            (end-segment (side2 rect))))))

(define (rect-width rect)
  (car (side-lengths rect)))
(define (rect-height rect)
  (cdr (side-lengths rect)))

(define s1 
  (make-segment (make-point 0 0)
                (make-point 0 5)))

(define s2
  (make-segment (make-point 5 0)
                (make-point 5 5)))

(rect-area (make-rect s1 s2))
