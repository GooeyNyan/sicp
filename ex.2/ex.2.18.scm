(define (reverse items)
  (define (reverse-iter items result)
    (if (null? items)
      result
      (reverse-iter (cdr items) (cons (car items) result))))
  (reverse-iter items '()))

(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

(define (reverse items)
  (if (null? items)
    '()
    (append (reverse (cdr items)) (list (car items)))))

(reverse (list 1 4 9 16 25))
