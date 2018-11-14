(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    '()
    (cons (accumulate op init (map (lambda (seq) (car seq)) seqs))
          (accumulate-n op init (map (lambda (seq) (cdr seq)) seqs)))))                                       
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (mi) (dot-product mi v)) m))
(define (transpose m)
  (accumulate-n cons '() m))
(define (matrix-*-matrix m n)7
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row))
         m)))

(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))                                                 
(define v (list 1 2 3 4))

(dot-product (car m) v)
(matrix-*-vector m v)
(transpose m)
(matrix-*-matrix m m)
