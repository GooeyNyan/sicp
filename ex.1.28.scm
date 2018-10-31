(define (prime? n)
  (fast-prime n 1000))

(define (fast-prime n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n) (fast-prime n (- times 1)))
        (else #f)))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  ; not equal to 1 or (- n 1), square is equal to 1 modulo n
  (define (square-check x) 
    (if (and (not (or (= x 1) (= x (- m 1))))
             (= (remainder (square x) m) 1))
      0
      (remainder (square x) m)))
  (cond ((= exp 0) 1)
        ((even? exp) (square-check (expmod base (/ exp 2) m)))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))
