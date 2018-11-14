(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
(define one (add-1 zero))
(define two (add-1 two))

(add-1 zero)
(lambda (f) (lambda (x) (f x)))
(((add-1 zero) (lambda (x) x)) 1)

(add-1 one)
(lambda (f) (lambda (x) (f ((one f) x))))
(lambda (f) (lambda (x) (f (f x))))

(define two (add-1 one))

(define (+ p1 p2)
  ; p1 p2 传入 f 之后展开
  (lambda (f) (lambda (x) ((p1 f) ((p2 f) x)))))

(((+ one two) square) 2)
