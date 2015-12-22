(define-library (automate eval)
  (import (scheme base)
	  (scheme eval)
	  (automate compiler)
	  (automate error))
  (export make-automate-environment
	  automate-environment?
	  return
	  current-state
	  current-label
	  set-label!
	  state-pop!
	  automate-eval)
  (include "eval.scm"))
