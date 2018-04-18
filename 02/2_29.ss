; 2.29 - Balancing Binary Mobiles

; i - constructors 
(define (make-mobile left right) (list left right))
(define (make-branch length structure) (list length structure))

; a - selectors
(define (left-branch  mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))
(define (branch-length    branch) (car branch))
(define (branch-structure branch) (cadr branch))

; b - total weight of a mobile 
(define (total-weight mobile)
    (+ (branch-weight (left-branch mobile)) 
       (branch-weight (right-branch mobile))))

(define (branch-weight branch)
  (cond ((mobile? branch) (total-weight (branch-structure branch)))
        (else (branch-structure branch))))

(define (mobile? branch) (pair? (branch-structure branch)))

; c - check if a mobile is balanced
(define (balanced? mobile)
  (and (weights-balance? mobile)
       (branch-balanced? (left-branch mobile))
       (branch-balanced? (right-branch mobile))))

(define (weights-balance? mobile)
  (= (* (branch-weight (left-branch mobile))
        (branch-length (left-branch mobile)))
     (* (branch-weight (right-branch mobile))
        (branch-length (right-branch mobile)))))

(define (branch-balanced? branch)
  (if (mobile? branch)      
      (balanced? (branch-structure branch))
      #t))

; testing
(define branch-1 (make-branch 4 1))
(define branch-2 (make-branch 3 4))
(define branch-3 (make-branch 4 (make-mobile branch-1 branch-2)))
(define branch-4 (make-branch 1 1))
(define branch-5 (make-branch 2 (make-mobile branch-4 branch-4)))

; should weigh 6, not balanced
(define testmobile-1 (make-mobile branch-3 branch-4))

; should weigh 3, be balanced
(define testmobile-2 (make-mobile branch-1 branch-5))