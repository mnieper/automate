(define-record-type state-type
  (%make-state label next-label)
  state?
  (label state-label state-set-label!)
  (next-label state-next-label state-set-next-label!))

(define (make-state label next-label)
  (%make-state label next-label))

(define (new-state state)
  (let ((next-label (state-next-label state)))
    (make-state next-label (+ next-label 1))))

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
  (%make-automate-environment return (list (make-state 0 1))))

(define (set-label! env label)
  (let*
      ((state (current-state env))
       (current-label (state-label state)))
    (unless (symbol? current-label)
      (state-set-next-label! state current-label))
    (state-set-label! state label)))

(define (state-push! env)
  (let ((current-state (current-state env)))
    (automate-environment-set-states! env (cons (new-state current-state)
						(automate-environment-states env)))))

(define (state-pop! env label)
  (let ((states (states env label)))
    (when (null? states) (automate-error "label not found" label))
    (automate-environment-set-states! env states)))

(define (current-label env)
  (state-label (current-state env)))

(define (automate-eval env code)
  (state-push! env)
  ((eval (compile code)
	 (environment '(scheme base)
		      '(automate error)
		      '(automate eval)))
   env))
