; subset
; 一个数的子集，
; 总是由除第一个元素的子集，加上这个子集和第一个元素的子集组成
(define (subset s)
  (if (null? s)
    (list '())
    (let ((rest (subset (cdr s))))
      (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subset (list 1 2 3))
