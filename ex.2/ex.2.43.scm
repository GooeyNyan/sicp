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

(define (prime? n)
  (define (prime-iter a n)
    (cond ((> a (sqrt n)) #t)
          ((= (remainder n a) 0) #f)
          (else (prime-iter (+ a 2) n))))
  (cond ((= n 2) #t)
        ((= (remainder n 2) 0) #f)
        (else (prime-iter 3 n))))

(define (prime-sum? pair)
  (newline)
  (display (prime? (+ (car pair) (cadr pair))))
  (display (+ (car pair) (cadr pair)))
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))


(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (flatmap (lambda (i)
                                     (map (lambda (j) (list i j))
                                          (enumerate-interval 1 (- i 1))))
                                   (enumerate-interval 1 n)))))

(prime-sum-pairs 10)

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

; this strategy reduces the problem of generating permutations of S to the problem of generating the permutations of sets with fewer element than S.
(define (permutations s)
  (if (null? s)
    (list '())
    (flatmap (lambda (x)
               (map (lambda (p) (cons x p))
                    (permutations (remove x s))))
             s)))

(permutations (list 1 2 3))

(define (queens board-size)
  (define empty-board '())
  (define (safe? col positions)
    (let ((k-1-th-queen (list-ref positions (- col 1)))
          (other-queens (filter (lambda (q)
                                  (not (= col (position-col q))))
                                positions)))
      (define (attacks? q1 q2)
        (or (= (position-row q1) (position-row q2))
            (= (abs (- (position-row q1) (position-row q2)))
               (abs (- (position-col q1) (position-col q2))))))))
  (define (make-position row col)
    (cons row col))
  (define (position-row position)
    (car row))
  (define (position-col position)
    (cdr row))
  (define (adjoin-position row col positions)
    (append positions (list (make-position row col))))
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
                   ; 加入新一列的各个位置
                   (adjoin-position
                     new-row k rest-of-queens))
                 ; queen-cols 和 enumerate-interval 交换次序的话，就由 linear-structure 变成 tree-structure 了
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
