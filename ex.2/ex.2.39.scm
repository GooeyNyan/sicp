(define (fold-right op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
          (fold-right op init (cdr sequence)))))        

(define (fold-left op init sequence)
  (define (iter result sequence)
    (if (null? sequence)
        result
        (iter (op result (car sequence))
              (cdr sequence))))
  (iter init sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (append (list y) x)) '() sequence))
; (define (reverse sequence)
;   (fold-right (lambda (x y) (append y (list x))) '() sequence))

(define s (list 1 2 3 4))
(reverse s)
