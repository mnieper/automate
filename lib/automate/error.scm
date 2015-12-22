(define-record-type automate-error-object-type
  (%make-automate-error-object message irritants)
  automate-error-object?
  (message automate-error-object-message)
  (irritants automate-error-object-irritants))

(define (make-automate-error-object message . obj*)
  (%make-automate-error-object message obj*))

(define (automate-error message . obj*)
  (raise (apply make-automate-error-object message obj*)))
