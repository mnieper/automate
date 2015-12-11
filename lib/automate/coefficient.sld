(define-library (automate coefficient)
  (import (scheme base))
  (export make-field
	  make-coefficient
	  coefficient?
	  coefficient-sum
	  coefficient-product
	  coefficient-zero?
	  coefficient-inverse
	  coefficient-negative
	  coefficient-negative-quotient
	  externalize-coefficient)
  (include "coefficient.scm"))
