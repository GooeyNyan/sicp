(define (filtered-accumulate predicate combiner null-value term next a b)
  (if (> a b)
    null-value
    (combiner 
      (if (predicate a) (term a) null-value)
      (filtered-accumulate predicate combiner null-value term next (next a) b))))

(define (prime? n)
  (if (not (even? n))
    (fast-prime n 100)
    (if (= n 2) #t #f)))

(define (fast-prime n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n) (fast-prime n (- times 1)))
        (else #f)))

(define (miller-rabin-test n)
  (define (try-it x)
    (= (expmod x (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (define (square-remainder x)
    (if (and (not (or (= x 1) (= x (- m 1))))
             (= (remainder (square x) m) 1))
      0
      (remainder (square x) m)))
  (cond ((= exp 0) 1)
        ((even? exp) (square-remainder (expmod base (/ exp 2) m)))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (sum-sqaure-primer a b)
  (filtered-accumulate prime? + 0 square (lambda (x) (+ x 1)) a b))

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (sum-positive-integer n)
  (filtered-accumulate 
    (lambda (x) (= (gcd x n) 1))
    +
    0
    (lambda (x) x)
    (lambda (x) (+ x 1))
    1
    (- n 1)))

(sum-sqaure-primer 2 1000)

(sum-positive-integer 6)
