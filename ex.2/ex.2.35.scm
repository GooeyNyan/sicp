(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

(define (count-leaves tree)
  (accumulate (lambda (x y) (+ x y))
              0
              (map (lambda (sub-tree)
                     (if (pair? sub-tree)
                       (count-leaves sub-tree)
                       1))
                   tree)))

(define seq1 (list 1 2 (list 3 4 (list 5 6))))

(count-leaves seq1)
