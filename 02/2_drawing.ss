; Vectors ------------------------------------------------------
; Ex. 2.46
(define (make-vect x y) (list x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cadr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

; Frames -------------------------------------------------------
; Ex. 2.47
(define (make-frame origin edge1 edge2) (list origin edge1 edge2))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame)  (cadr frame)) 
(define (edge2-frame frame)  (caddr frame)) 

(define (frame-coord-map frame)
  (lambda (v) 
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v)
                            (edge1-frame frame))
                (scale-vect (ycor-vect v)
                            (edge2-frame frame))))))

; Segments ------------------------------------------------------
(define (make-segment start-v stop-v) (list start-v stop-v))
(define (start-segment segment) (car segment))
(define (stop-segment  segment) (cadr segment))

; Segments - Actual patterns -------------------------------------
; Ex. 2.49.b
(define segments-x
  (list (make-segment (make-vect 0.0 0.0) (make-vect 1.0 1.0))
        (make-segment (make-vect 0.0 1.0) (make-vect 1.0 0.0))))

(define segments-wave
  (list (make-segment (make-vect 0.00 0.85) (make-vect 0.16 0.60))   ; 1
        (make-segment (make-vect 0.16 0.60) (make-vect 0.30 0.65))   ; 2
        (make-segment (make-vect 0.30 0.65) (make-vect 0.40 0.65))   ; 3
        (make-segment (make-vect 0.40 0.65) (make-vect 0.35 0.85))   ; 4
        (make-segment (make-vect 0.35 0.85) (make-vect 0.40 1.00))   ; 5
        (make-segment (make-vect 0.60 1.00) (make-vect 0.65 0.85))   ; 6
        (make-segment (make-vect 0.65 0.85) (make-vect 0.60 0.65))   ; 7
        (make-segment (make-vect 0.60 0.65) (make-vect 0.76 0.65))   ; 8
        (make-segment (make-vect 0.76 0.65) (make-vect 1.00 0.36))   ; 9
        (make-segment (make-vect 1.00 0.15) (make-vect 0.60 0.45))   ; 10
        (make-segment (make-vect 0.60 0.45) (make-vect 0.76 0.00))   ; 11
        (make-segment (make-vect 0.60 0.00) (make-vect 0.51 0.29))   ; 12
        (make-segment (make-vect 0.51 0.29) (make-vect 0.40 0.00))   ; 13
        (make-segment (make-vect 0.25 0.00) (make-vect 0.35 0.50))   ; 14
        (make-segment (make-vect 0.35 0.50) (make-vect 0.30 0.60))   ; 15
        (make-segment (make-vect 0.30 0.60) (make-vect 0.15 0.40))   ; 16
        (make-segment (make-vect 0.15 0.40) (make-vect 0.00 0.65)))) ; 17

; Painters and their Transforms ----------------------------------
(define (segments->painter device segment-list)
  ; Returns a function that uses device to draw segments onto frame.
  (lambda (frame) 
    (for-each 
        (lambda (segment) 
            (draw-line device 
                ((frame-coord-map frame) (start-segment segment))
                ((frame-coord-map frame) (stop-segment  segment))))
            segment-list)))

(define (transform-painter painter origin corner1 corner2)
  ; Basis for all subsequent transforms.
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter 
          (make-frame new-origin
                      (sub-vect (m corner1) new-origin)
                      (sub-vect (m corner2) new-origin)))))))

; - Transforms: Basic
(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
; Ex. 2.50a
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))   

(define (rotate90  painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))   
; Ex 2.50b
(define (rotate180 painter)
  (rotate90 (rotate90 painter)))
; Ex 2.50c
(define (rotate270 painter)
  (rotate90 (rotate180 painter)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))   

; - Transforms: Composite
(define (besides painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left 
            (transform-painter painter1
                               (make-vect 0.0 0.0)
                               split-point
                               (make-vect 0.0 1.0)))
          (paint-right 
            (transform-painter painter2
                               split-point
                               (make-vect 1.0 0.0)
                               (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

; Ex. 2.51
(define (below   painter1 painter2)
  (let ((paint-top 
            (transform-painter painter1
                               (make-vect 0.0 0.5)
                               (make-vect 1.0 0.5)  ; Key here is the vectors are relative to the OLD origin. 
                               (make-vect 0.0 1.0)))
          (paint-bottom 
            (transform-painter painter2
                               (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               (make-vect 0.0 0.5))))
      (lambda (frame)
        (paint-top frame)
        (paint-bottom frame))))

(define (right-split painter n) 
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (besides painter (below smaller smaller)))))

; Ex. 2.44
(define (up-split painter n)      
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        ; the parameter order depends on your implementations of besides and below.
        (below (besides smaller smaller) painter))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (besides up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (besides (below top-left painter)
                   (below corner bottom-right))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (besides (flip-horiz quarter) quarter)))
      (below half (flip-vert half)))))

; Drawing Utilities ---------------------------------------------
(define (get-device)
    (define geo-string (x-geometry-string #f #f 1000 1000))
    (make-graphics-device 'x #f geo-string #f))

(define (draw-line device p1 p2)
  (graphics-draw-line device (car p1) (cadr p1) (car p2) (cadr p2)))

(define (test)
    (define device (get-device))    
    (define painter (segments->painter device segments-wave))

    (define initial-frame (make-frame (make-vect -1.0 -1.0)
                                      (make-vect 2.0 0.0)
                                      (make-vect 0.0 2.0)))

    ; Make a crazy painter with our new language!
    (define test-comp-painter 
      (besides painter 
              (rotate90 (besides painter 
                                 painter))))

    ; Set it off on the initial frame. 
    ;(test-comp-painter initial-frame)
    ;((corner-split painter 4) initial-frame)
    ((square-limit painter 5) initial-frame)
    ;(painter initial-frame)

    (define port (current-input-port))
    (define (rec-char? port) 
        (if (not (char-ready? port)) 
            (rec-char? port)
            ()))  
    ; need to send an actual double quoted char.
    (rec-char? port)  
    (graphics-close device))