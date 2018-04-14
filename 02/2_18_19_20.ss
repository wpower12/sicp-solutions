(define squares (list 1 4 9 16 25 36 49 64 81))

(define up-to-10 (list 1 2 3 4 5 6 7 8 9 10))

(define (square x)
  (* x x))

; 2.17 - Last Pair
(define (last-pair list)	
  (if (null? (cdr list))
      (car list)
      (last-pair (cdr list))))

; 2.18 - Reverse
(define (reverse list-in)
  (define (reverse-rec list ret)
    (if (null? list)
        ret
        (reverse-rec (cdr list) (cons (car list) ret))))
  (reverse-rec list-in ()))

(define (reverse-2 list)
  (if (null? list)
      ()
      (cons (reverse-2 (cdr list)) (car list))))

; 2.20 - Same Parity 
(define (same-parity . ns)
	(define (parity-rec n list ret)
		(cond ((null? list) ret)
		      ((= (modulo n 2) (modulo (car list) 2)) 
		      		(parity-rec n (cdr list) (cons (car list) ret)))
		  	  (else (parity-rec n (cdr list) ret))))
	(reverse (parity-rec (car ns) ns ())))