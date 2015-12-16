(define-record-type ordering-type
  (make-ordering monomial monomial? >? exponent product quotient syzygy)
  ordering?
  (monomial %make-monomial)
  (monomial? %monomial?)
  (>? %monomial>?)
  (exponent %monomial-exponent)
  (product %monomial-product)
  (quotient %monomial-quotient)
  (syzygy %monomial-syzygy))

(define (make-monomial ordering exponent)
  ((%make-monomial ordering) exponent))

(define (monomial? ordering monomial)
  ((%monomial? ordering) monomial))

(define (monomial>? ordering monomial1 monomial2)
  ((%monomial>? ordering) monomial1 monomial2))

(define (monomial-exponent ordering monomial)
  ((%monomial-exponent ordering) monomial))

(define (monomial-product ordering dim monomial1 monomial2)
  ((%monomial-product ordering) dim monomial1 monomial2))

(define (monomial-quotient ordering monomial1 monomial2)
  ((%monomial-quotient ordering) monomial1 monomial2))

(define (monomial-syzygy ordering monomial1 monomial2)
  ((%monomial-syzygy ordering) monomial1 monomial2))

(define (externalize-indeterminate indeterminates i)
  (list-copy (vector-ref indeterminates i)))

(define (externalize-monomial ordering indeterminates monomial)
  (let ((exponent (monomial-exponent ordering monomial)))
    (do	((i (- (vector-length exponent) 1) (- i 1))
	 (d '()
	    (let ((e (vector-ref exponent i)))
	      (case e
		((0) d)
		((1) (cons (externalize-indeterminate indeterminates i) d))
		(else (cons (externalize-indeterminate indeterminates i) (cons e d)))))))
	((= i -1)
	 (cond
	  ((null? d) 1)
	  ((null? (cdr d)) (car d))
	  (else d))))))
