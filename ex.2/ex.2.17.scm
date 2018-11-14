(define (last-pair items)
  (let ((next (cdr items)))
    (if (null? next)
      (car items)
      (last-pair next))))

(last-pair (list 1 2 3 4))
