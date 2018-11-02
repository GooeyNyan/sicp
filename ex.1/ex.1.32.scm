(define (accumulate combiner null-value term next a b)
  (if (> a b)
    null-value
    (combiner (term a) (accumulate combiner null-value term next (next a) b))))

(define (accumulate combiner null-value term next a b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum term next a b)
  (accumulate + 0 term next a b))

(define (product term next a b)
  (accumulate * 1 term next a b))

(define (sum-integer a b)
  (sum (lambda (x) x) (lambda (x) (+ x 1)) a b))

(sum-integer 1 10)
