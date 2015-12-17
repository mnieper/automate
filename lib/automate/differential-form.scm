(define (make-differential-algebra field ordering dim)
  (make-algebra field ordering dim))

(define (externalize-differential-form algebra indeterminates polynomial)
  (let
      ((indeterminates (vector-append indeterminates
				      (vector-map (lambda (i)
						    (list 'd i))
						  indeterminates))))
  (externalize-polynomial algebra indeterminates polynomial)))
