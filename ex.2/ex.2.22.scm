(define (square-list items)
  (if (null? items)
    '()
    (cons (square (car items))
          (square-list (cdr items)))))

(define (square-list items)
  (map square items))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            ; 后面的 answer 反而在前面，所以反了
            (cons (square (car things))
                  answer))))
  (iter items '()))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            ; did not obey the box-the-pointer
            (cons answer
                  (square (car things))))))
  (iter items '()))

(square-list (list 1 2 3 4))
