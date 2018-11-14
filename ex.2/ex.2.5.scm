(define (exp base power)
  (define (iter a times result)
    (cond ((= times 0) result)
          ((even? times) (iter (square a) (/ times 2) result))
          (else (iter a (- times 1) (* a result)))))
  (iter base power 1))
(define (count-0-remainder-divisions z d)
  (define (iter count)
    (if (= (remainder z (exp d count)) 0)
      (iter (+ count 1))
      (- count 1)))
  (iter 0))

(define (count-0-remainder-divisions z count d)
  (if (= (remainder z d) 0)
    (count-0-remainder-divisions (/ z d) (+ count 1) d)
    count))
(define (cons a b)
  (* (exp 2 a) (exp 3 b)))
(define (car z)
  (count-0-remainder-divisions z 0 2))
(define (cdr z)
  (count-0-remainder-divisions z 0 3))

(define (car z)
  (define (divisble-by-3? z)
    (= (remainder z 3) 0))
  (if (divisble-by-3? z)
    (car (/ z 3))
    (/ (log z) (log 2))))
(define (cdr z)
  (define (divisble-by-2? z)
    (even? z))
  (if (divisble-by-2? z)
    (car (/ z 2))
    (/ (log z) (log 3))))

(define (largest-power-of z d)
  (if (= (remainder z d) 0)
    (+ 1 (largest-power-of (/ z d) d))
    0))
(define (car z)
  (largest-power-of z 2))
(define (cdr z)
  (largest-power-of z 3))

(define z1 (* (exp 2 0) (exp 3 4)))
(car z1)
(cdr z1)
