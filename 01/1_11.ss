(define (f-iter n)
	(cond ((= n 0) 0)
    	((= n 1) 1)
		((= n 2) 2)
  		(else (f-iter-inner 0 1 2 n 3))))

; Iterative/Tail Recursive
(define (f-iter-inner n-3 n-2 n-1 target n)
	(if (>= n target)
		(+ n-1 (+ (* 2 n-2) (* 3 n-3)))	; then: return f(n), else:
		(f-iter-inner n-2							  ; new n-3
					  n-1							  ; new n-2	
					  (+ n-1 (+ (* 2 n-2) (* 3 n-3))) ; new n-1 = f(n)
					  target 
					  (+ n 1))))

; Most def. not linear recursive. Big ol' tree of saved state.
(define (f-rec n)	
  (cond ((= n 0) 0)
        ((= n 1) 1)
		((= n 2) 2)
		(else (+ (f-rec (- n 1)) (+ (* 2 (f-rec (- n 2))) (* 3 (f-rec (- n 3))))))))