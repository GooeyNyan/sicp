(define (f n)
  (if (< n 3)
    n
    (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

; (define (f n)
;   (define (iter a b c count)
;     (if (= count 0)
;       b
;       (iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
;   (if (< n 3)
;     n
;     (iter 2 1 0 (- n 1))))

(f 1)
(f 2)
(f 3)
(f 4)
(f 5)
(f 6)
(f 7)
(f 8)
(f 9)
