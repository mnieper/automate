(define-library (automate rational-field)
  (import (scheme base)
	  (automate coefficient))
  (export exact-rational? rational-field)
  (include "rational-field.scm"))
