; 1.29 - Simpsons Rule Procedure

; integral for comparison
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x)
  (* x x x))

(define (add-2 x)
  (+ x 2))

(define (simp-rule f a b n)
  (let ((h (/ (- b a) 2)) )
    (define (simp-odd x)  (* 4 (f (+ a (* h x)))))
  (define (simp-even x) (* 2 (f (+ a (* h x)))))
    (* (/ h 3.0) 
       (+ (f a)
          (f b)
          (sum simp-odd  1 add-2 (- b 1))    ; odd sum, starting at 1  4*f
          (sum simp-even 2 add-2 (- b 2)))))) ; even sum, starting at 2 2*f