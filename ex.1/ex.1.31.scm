(define (product term next a b)
  (if (> a b)
    1
    (* (term a) (product term next (next a) b))))

(define (product term next a b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* (term a) result))))
  (iter a 1))

(define pi
  (* 4 (product 
         (lambda (x) (if (even? x)
                       (/ (+ x 2) (+ x 1))
                       (/ (+ x 1) (+ x 2))))
         (lambda (x) (+ x 1))
         1
         150000)))

(* 1.0 pi)
