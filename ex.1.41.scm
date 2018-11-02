(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ x 1))

; (double double) 之后是 4 次
; 再 double 翻倍就是 4 * 4 = 16 次
(((double (double double)) inc) 5)
