; 2.28 - Fringe - Return a left-to-right list of all leaves of the tree.
(define (fringe tree)
  (cond ((null? tree) ())
        ((not (pair? tree)) (list tree))
    	(else (append (fringe (car tree)) (fringe (cdr tree))))))