(define (filter predicat sequence)
  (if (null? sequence)
    '()
    (if (predicat (car sequence))
      (cons (car sequence) (filter predicat (cdr sequence)))
      (filter predicat (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (enumerate-interval a b)
  (if (> a b)
    '()
    (append (list a) (enumerate-interval (+ a 1) b))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (make-position row col)
  (cons row col))

(define (position-row position)
  (car position))

(define (position-col position)
  (cdr position))

(define empty-board '())

(define (adjoin-position row col queen-positions)
  (append queen-positions (list (make-position row col))))

(define (safe? col queen-positions)
  (let ((kth-queen (list-ref queen-positions (- col 1)))
        (other-queens (filter (lambda (queen)
                                (not (= col (position-col queen))))
                              queen-positions)))
    (define (attacks? queen1 queen2)
      (or (= (position-row queen1) (position-row queen2))
          (= (abs (- (position-row queen1) (position-row queen2)))
             (abs (- (position-col queen1) (position-col queen2))))))
    (define (iter queen other-queens)
      (or (null? other-queens)
          (and (not (attacks? queen (car other-queens)))
               (iter queen (cdr other-queens)))))
    (iter kth-queen other-queens)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      ; empty-board represents an empty set of positions
      (list empty-board)
      (filter
        ; safe? determines for a set of positions,
        ; whether the queen in the kth column is safe with respect to the others.
        (lambda (positions) (safe? k positions))
        (flatmap
          ; rest-of-queens is a way to place k-1 queens in the first k-1 columns.
          (lambda (rest-of-queens)
            ; new-row is a proposed row in which to place the queen for the kth column.
            (map (lambda (new-row)
                   ; adjoin-position adjoins a new row-column position to a set of position.
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 1)
(queens 2)
(queens 3)
(queens 8)
