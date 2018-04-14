(define (good-enough? guess x)  
  (< (abs (- (* guess guess) x)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))


(define (sqrt-iter guess x)
  (if (good-enough? guess x)  
      guess 
      (sqrt-iter (improve guess x)
            x)))
        
(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (new-if pred-c then-c else-c)
  (cond (pred-c then-c)
        (else else-c)))

; ** 1.6
(define (new-sqrt x)
  (new-sqrt-iter 1.0 x))

(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)  
      guess 
      (sqrt-iter (improve guess x)
            x)))

; ** 1.8
(define (cube-root x)
  (cube-iter 1.0 x))

(define (good-enough-cube? guess x)
  (< (abs (- (* guess (* guess guess)) x)) 0.001))

(define (improve-cube guess x)
  (/ (+ (* 2 guess) (/ x (* guess guess))) 3))

(define (cube-iter guess x)
  (if (good-enough-cube? guess x)
      guess
      (cube-iter (improve-cube guess x)
                  x)))
