(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

; 2 的 n 次方
(A 1 10)

; ((((2^2)^2)^2)^2)
(A 2 4)
; (A 1 (A 2 3))
; (A 1 (A 1 (A 2 2)))
; (A 1 (A 1 (A 1 (A 2 1))))
; ; 2 的 2 次方
; (A 1 (A 1 (A 1 2)))
; (A 1 (A 1 4))
; (A 1 16)

(A 3 3)
; (A 2 (A 3 2))
; (A 2 (A 2 (A 3 1)))
; (A 2 (A 2 2))
; (A 2 (A 1 (A 2 1)))
; (A 2 4)

; 2n
(define (f n) (A 0 n))
; 2^n
(define (g n) (A 1 n))
; a(n+1) = 2^a(n)
(define (h n) (A 2 n))
(define (k n) (* 5 n n))
