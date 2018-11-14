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
(define s (list 1 2 3 4))
(fold-right / 1 s)
(fold-left / 1 s)
(fold-right cons '() s)
(fold-left cons '() s)

(fold-right + 0 s)
(fold-left + 0 s)
(fold-right * 1 s)
(fold-left * 1 s)
