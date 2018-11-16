
; Helpers
(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

; Constructors
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

; Getters/Checkers
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

; -- Problem 2.56 - Exponentiation
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '^)))

(define (make-exponent base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '^ base exponent))))

(define (base exp) (cadr exp))

(define (exponent exp) (caddr exp))

; Symbolic Differentiation
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) 
         (if (same-variable? exp var) 1 0))
        ((sum? exp) 
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp) 
         (make-sum
            (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp))))
        ; Problem 2.56
        ((exponentiation? exp)
         (make-product
          (deriv (base exp) var)
          (make-product 
           (exponent exp)
           (make-exponent (base exp) (- (exponent exp) 1)))))
        (else 
         (error "unknown expression type -- DERIV" exp))))

;Testing
(define (test1)
  (deriv '(* x 3) 'x))

(define (test2)
  (deriv '(^ x 3) 'x))

; Trying to get this down to just (* 4 x)
; Best ive gotten is (* 2 (* 2 x))
(define (test3)
  (deriv '(* 2 (^ x 2)) 'x))

(define (test4)
  (deriv '(^ (+ (^ x 2) x) 2) 'x))