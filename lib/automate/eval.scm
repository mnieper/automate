(define-record-type state-type
  (%make-state label next-label characteristic globals locals)
  state?
  (label state-label state-set-label!)
  (next-label state-next-label state-set-next-label!)
  (characteristic state-characteristic state-set-characteristic!)
  (globals state-globals state-set-globals!)
  (locals state-locals state-set-locals!))

(define (make-state label next-label characteristic globals locals)
  (%make-state label next-label characteristic globals locals))

(define (new-state state)
  (let ((next-label (state-next-label state)))
    (make-state next-label
		(+ next-label 1)
		(state-characteristic state)
		(list-copy (state-globals state))
		(list-copy (state-locals state)))))

(define-record-type automate-environment-type
  (%make-automate-environment return states)
  automate-environment?
  (return return)
  (states automate-environment-states automate-environment-set-states!))

(define (current-state env)
  (car (automate-environment-states env)))

(define (states env label)
  (let loop ((states (automate-environment-states env)))
    (cond
     ((null? states) '())
     ((eq? label (state-label (car states))) states)
     (else (loop (cdr states))))))

(define (make-automate-environment return)
  (%make-automate-environment return (list (make-state 0
						       1
						       #f
						       '()
						       '()))))

(define (set-label! env label)
  (let*
      ((state (current-state env))
       (current-label (state-label state)))
    (unless (symbol? current-label)
      (state-set-next-label! state current-label))
    (state-set-label! state label)))

(define (set-characteristic! env p)
  (let ((state (current-state env)))
    (when (state-characteristic state)
	  (automate-error "characteristic already set to" (state-characteristic state)))
    (state-set-characteristic! state p)))

(define (add-global! env var)
  (let*
      ((state (current-state env))
       (globals (state-globals state))
       (locals (state-locals state)))
    (when (or (memq var globals) (memq var locals))
	  (automate-error "variable already declared" var))
    (state-set-globals! state (cons var globals))))

(define (add-local! env var)
  (let*
      ((state (current-state env))
       (globals (state-globals state))
       (locals (state-locals state)))
    (when (or (memq var globals) (memq var locals))
	  (automate-error "variable already declared" var))
    (state-set-locals! state (cons var locals))))

(define (state-push! env)
  (let ((current-state (current-state env)))
    (automate-environment-set-states! env (cons (new-state current-state)
						(automate-environment-states env)))))

(define (state-pop! env label)
  (let ((states (states env label)))
    (when (null? states) (automate-error "label not found" label))
    (automate-environment-set-states! env states)))

(define (variable env)
  (let ((state (current-state env)))
    ...))

(define (current-label env)
  (state-label (current-state env)))

(define (automate-eval env code)
  (state-push! env)
  ((eval (compile code)
	 (environment '(scheme base)
		      '(automate error)
		      '(automate eval)))
   env))
