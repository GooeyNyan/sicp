(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (filter predicat sequence)
  (if (null? sequence)
    '()
    (if (predicat (car sequence))
      (cons (car sequence) (filter predicat (cdr sequence)))
      (filter predicat (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (flat sequence)
  (accumulate append '() sequence))

(define (enumerate-interval a b)
  (if (> a b)
    '()
    (append (list a) (enumerate-interval (+ a 1) b))))

(define (filter-not-null sequence)
  (filter (lambda (x) (not (null? x))) sequence))

; generate 1 <= k < j < i <= n ordered triples
(define (ordered-triples n)
  (flat
    (filter-not-null
      (flatmap (lambda (i)
                 (map (lambda (j)
                        (map (lambda (k) (list k j i))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
               (enumerate-interval 1 n)))))

(define (sum-ordered-triples triples)
  (accumulate + 0 triples))

(define (sum-of-ordered-triples-equal-to-s n s)
  (filter (lambda (triples) (= (sum-ordered-triples triples) s))
          (ordered-triples n)))

(ordered-triples 10)
(sum-of-ordered-triples-equal-to-s 10 11)
