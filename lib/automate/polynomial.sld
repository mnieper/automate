(define-library (automate polynomial)
  (import (scheme base)
	  (automate coefficient)
	  (automate monomial))
  (export make-term term? term-coefficient term-monomial
	  make-polynomial polynomial?
	  polynomial-zero? leading-term polynomial-remainder
	  polynomial-sum
	  polynomial-product
	  externalize-polynomial
	  reduce
	  groebner-basis)
  (include "polynomial.scm"))
