(define-library (automate monomial)
  (import (scheme base))
  (export make-ordering
	  make-monomial
	  monomial?
	  monomial>?
	  monomial-exponent
	  monomial-product
	  monomial-quotient
	  monomial-syzygy
	  externalize-monomial)
  (include "monomial.scm"))
