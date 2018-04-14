(define (timed-prime-test n)
  (display n)
  (start-prime-test n (system-clock)))

(define (start-prime-test n start-time)		
  (if (prime? n)	
      (begin 
      	(report-prime (- (system-clock) start-time))
      	#t)
	   #f))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
    	(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)	
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

; Script for experiments
(define (tsl n)
  (smallest-iter 0 n))

(define (smallest-iter count n)
  (newline)	
  ; If i dont make this do something, the times are too small.
  (if (< count 3)
      (if (timed-prime-test n) 
      	(smallest-iter (+ 1 count) (+ 2 n))
      	(smallest-iter count (+ 2 n)))))

 ;(display n)
 ; (newline)	
 ; (if (< count 3)
 ;     (if (prime? n) 
 ;     	(begin 
 ;     		(timed-prime-test n)
 ;     		(smallest-iter (+ 1 count) (+ 2 n)))
 ;     	(smallest-iter count (+ 2 n)))))

