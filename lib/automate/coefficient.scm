(define-record-type field-type
  (make-field coefficient coefficient? externalize sum product zero? inverse)
  field?
  (coefficient %make-coefficient)
  (coefficient? %coefficient?)
  (externalize %externalize-coefficient)
  (sum %coefficient-sum)
  (product %coefficient-product)
  (zero? %coefficient-zero?)
  (inverse %coefficient-inverse))

(define (make-coefficient field number)
  ((%make-coefficient field) number))

(define (coefficient? field number)
  ((%coefficient? field) number))

(define (externalize-coefficient field coefficient)
  ((%externalize-coefficient field) coefficient))

(define (coefficient-sum field summand1 summand2)
  ((%coefficient-sum field) summand1 summand2))

(define (coefficient-product field factor1 factor2)
  ((%coefficient-product field) factor1 factor2))

(define (coefficient-zero? field coefficient)
  ((%coefficient-zero? field) coefficient))

(define (coefficient-inverse field coefficient)
  ((%coefficient-inverse field) coefficient))

(define (coefficient-negative field coefficient)
  (coefficient-product field (make-coefficient field -1) coefficient))

(define (coefficient-negative-quotient field coefficient1 coefficient2)
  (coefficient-product field
		       (make-coefficient field -1)
		       (coefficient-product field
					    coefficient1
					    (coefficient-inverse field
								 coefficient2))))
