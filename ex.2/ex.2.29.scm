(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))
(define (branch-structure branch)
  (cdr branch))

; 开心，递归的声明式还是简洁
(define (total-weight mobile)
  (if (not (pair? mobile))
    mobile
    (+ (total-weight (branch-structure (left-branch mobile)))
       (total-weight (branch-structure (right-branch mobile))))))

; 声明式，描述我想干嘛就行了，就跟说话一样
(define (balanced? mobile)
  (if (pair? mobile)
    (and (= (* (branch-length (left-branch mobile))
               (total-weight (branch-structure (left-branch mobile))))
            (* (branch-length (right-branch mobile))
               (total-weight (branch-structure (right-branch mobile)))))
         (balanced? (branch-structure (left-branch mobile)))
         (balanced? (branch-structure (right-branch mobile))))
    #t))

(define a (make-branch 1 2))
(define b (make-branch 2 1))
(define m1 (make-mobile a b))

(total-weight m1)

;   m1
; m2 m3

(define m2 (make-mobile a b))
(define m1 (make-mobile (make-branch 1 m2)
                        (make-branch 1 m2)))
(define m3 (make-mobile (make-branch 3 m2)
                        (make-branch 1 m2)))

(balanced? m1)
(balanced? m3)
