(define (count-change amount)
  (define (count-change-iter amount kinds-of-coins)
    ; 关键在于终止条件
    ; 一 amount 为 0 说明正好够一种
    ; 二 amount < 0, kinds-of-coins = 0 说明木得了
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0)
          (else (+ (count-change-iter (- amount (first-denomination kinds-of-coins)) kinds-of-coins)
                   (count-change-iter amount (- kinds-of-coins 1))))))
  ; 各种剩余币种对应的面值
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))
  (count-change-iter amount 5))

(count-change 1000)
