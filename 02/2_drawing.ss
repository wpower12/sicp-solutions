; Vectors ------------------------------------------------------
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

; Segments - Actual patterns - includes ans. to 2.49 ------------
(define segments-x
  (list (make-segment (make-vect 0.0 0.0) (make-vect 1.0 1.0))
        (make-segment (make-vect 0.0 1.0) (make-vect 1.0 0.0))))


; Painters and their Transforms ----------------------------------
(define (segments->painter device segment-list)
  ; Returns a function that uses device to draw segments onto frames.
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

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))   

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))   

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

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-top 
            (transform-painter painter1
                               split-point
                               (make-vect 1.0 0.0)
                               (make-vect 0.0 1.0)))
          (paint-bottom 
            (transform-painter painter2
                               (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               split-point)))
      (lambda (frame)
        (paint-top frame)
        (paint-bottom frame)))))

(define (below-2 painter1 painter2)
  (rotate90 (besides painter1 painter2)))
 
(define (right-split painter n) 
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (besides painter (below smaller smaller)))))


; Drawing Utilities ---------------------------------------------
(define (get-device)
    (define geo-string (x-geometry-string #f #f 600 600))
    (make-graphics-device 'x #f geo-string #f))

(define (draw-line device p1 p2)
  (graphics-draw-line device (car p1) (cadr p1) (car p2) (cadr p2)))

(define (test)
    (define device (get-device))    
    (define painter (segments->painter device segments-x))

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
    ((right-split painter 4) initial-frame)

    (define port (current-input-port))
    (define (rec-char? port) 
        (if (not (char-ready? port)) 
            (rec-char? port)
            ()))  
    ; need to send an actual double quoted char.
    (rec-char? port)  
    (graphics-close device))