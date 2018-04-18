(define (draw-test-line device)
   (let ((x-start 0.0)
		 (y-start 0.0)
		 (x-end 5.0)
		 (y-end 5.0))
	(graphics-draw-line device x-start y-start x-end y-end)))

(define (get-device)
	(define geo-string (x-geometry-string #f #f 600 600))
	(make-graphics-device 'x #f geo-string #f))

(define (draw-till-keypress)
	(define port (current-input-port))
	(define device (get-device))

	(draw-test-line device)
	
	(define (rec-char? port) 
		(if (not (char-ready? port))
			(rec-char? port)
			()))
	
	; need to send an actual double quoted char.
	(rec-char? port) 
	
	(graphics-close device))