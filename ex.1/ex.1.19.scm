(define (fib n)
  (define (iter a b count)
    (if (= count 0) b
      (iter (+ a b) a (- count 1)))))

; (define (fib n)
;   (define (fib-iter a b p q count)
;     (cond ((= count 0) b)
;           ((even? count)
;            (fib-iter a b 
;                      (+ (* p p) (* q q))
;                      (+ (* q q) (* 2 p q))
;                      (/ count 2)))
;           (else (fib-iter (+ (* b q) (* a q) (* a p))
;                           (+ (* b p) (* a q))
;                           p
;                           q
;                           (- count 1)))))
;   (fib-iter 1 0 0 1 n))

(fib 10000)

; a
; bq + aq + ap

; b
; bp + aq

; 2 times
; (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
; bpq + aq2 + bq2 + aq2 + apq + bpq + apq + ap2
; (pq + q2 + pq)b + (q2 + q2 + pq + pq + p2)a
; (q2 + 2pq)b + (2q2 + 2pq + p2)a

; (bp + aq)p + (bq + aq + ap)q
; bp2 + apq + bq2 + aq2 + apq
; (p2 + q2)b + (2pq + q2)a

; p' = p2 + q2
; q' = q2 + 2pq
