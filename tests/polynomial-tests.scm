(import (scheme base)
	(automate rational-field)
	(automate grevlex-ordering)
	(automate coefficient)
	(automate monomial)
	(automate polynomial))

(test-begin "polynomial")

(define alg (make-algebra rational-field grevlex-ordering 0))

(define (show p)
  (externalize-polynomial alg
			  #(x y z)
			  p))

(define p
  (make-polynomial alg
		   (list
		    (make-term 1 (make-monomial grevlex-ordering #(4 0 0))))))

(define p1
  (make-polynomial alg
		   (list
		    (make-term 4 (make-monomial grevlex-ordering #(2 0 1)))
		    (make-term -2 (make-monomial grevlex-ordering #(2 0 0)))
		    (make-term -1 (make-monomial grevlex-ordering #(0 0 2)))
		    (make-term -1/2 (make-monomial grevlex-ordering #(1 0 0)))
		    (make-term 1/4 (make-monomial grevlex-ordering #(0 0 1))))))

(define p2
  (make-polynomial alg
		   (list
		    (make-term 2 (make-monomial grevlex-ordering #(2 0 1)))
		    (make-term 2 (make-monomial grevlex-ordering #(0 2 0)))
		    (make-term 1 (make-monomial grevlex-ordering #(1 0 0))))))
						
(test-equal '(+ (4 x 2 z) (-2 x 2) (-1 z 2) (-1/2 x) (1/4 z))
	    (show p1))

(test-equal '(+ (2 x 2 z) (2 y 2) x)
	    (show p2))

(define i (list p2))

(test-equal '(+ (-2 x 2) (-4 y 2) (-1 z 2) (-5/2 x) (1/4 z))
	    (show (reduce alg
			  i
			  p1)))

(test-end "polynomial")
