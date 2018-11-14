(define (fringe x)
  (cond ((null? x) '())
        ((not (pair? x)) (list x))
        (else (append (fringe (car x))
                      (fringe (cdr x))))))

(fringe (list (list 1 2)
              (list 3 4)
              (list 5 6)
              (list 7 8 
                    (list 9 10))))
