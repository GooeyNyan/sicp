(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

(define (reverse items)
  (if (null? items)
    '()
    (append (reverse (cdr items)) (list (car items)))))

(define (deep-reverse items)
  (if (null? items)
    '()
    (if (pair? (car items))
      (append (deep-reverse (cdr items)) (list (deep-reverse (car items))))
      (append (deep-reverse (cdr items)) (list (car items))))))
(define x (list (list 1 2) (list 3 4)))

(reverse x)
(deep-reverse x)
