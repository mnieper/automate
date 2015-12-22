(define-library (automate error)
  (import (scheme base))
  (export automate-error-object?
	  automate-error-object-message
	  automate-error-object-irritants
	  automate-error)
  (include "error.scm"))
