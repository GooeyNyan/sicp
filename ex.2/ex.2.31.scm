(define (square-tree tree)
  (tree-map square tree))

(define (tree-map term tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (term tree))
        (else (cons (tree-map term (car tree))
                    (tree-map term (cdr tree))))))

(define (tree-map term tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (tree-map term sub-tree)
           (term sub-tree)))
       tree))

(square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))
