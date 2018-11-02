; (define (sum term next a b)
;   (if (> a b)
;     0
;     (+ (term a)
;        (sum term next (next a) b))))

(define (sum term next a b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ (term a) result))))
  (iter a 0))

(define ())

(define (simpson-rule f a b n)
  (let ((h (/ (- b a) n)))
    (define (y k)
      (f (+ a (* k h))))
    (define (mul k v)
      (cond ((or (= k 0) (= k n)) 
             (* 1 v))
            ((even? k) (* 2 v))
            (else (* 4 v))))
    (* (/ h 3) (sum 
                 (lambda (k) (mul k (y k)))
                 (lambda (x) (+ x 1))
                 0
                 n))))

(define (cube x) (* x x x))
(simpson-rule cube 0 1 1000)
