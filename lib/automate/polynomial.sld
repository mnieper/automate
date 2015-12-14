(define-library (automate polynomial)
  (import (scheme base)
	  (automate coefficient)
	  (automate monomial))
  (export make-algebra field ordering
	  make-term term? term-coefficient term-monomial
	  make-polynomial polynomial?
	  zero-polynomial
	  polynomial-zero? leading-term polynomial-remainder
	  polynomial-sum
	  polynomial-product
	  externalize-polynomial
	  reduce
	  groebner-basis)
  (include "polynomial.scm"))
