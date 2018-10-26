(define (parcal row col)
  (cond ((< row 1) 0)
        ((or (< col 1) (> col row)) 0)
        ((and (= row 1) (= col 1)) 1)
        (else (+ (parcal (- row 1) (- col 1)) (parcal (- row 1) col)))))

(parcal 1 1)
(parcal 4 2)
