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
	  set-characteristic!
	  add-global!
	  add-local!
	  variable
	  state-pop!
	  automate-eval)
  (include "eval.scm"))
