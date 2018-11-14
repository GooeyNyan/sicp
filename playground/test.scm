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
