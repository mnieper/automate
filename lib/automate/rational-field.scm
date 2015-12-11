(define (exact-rational? number)
  (and (exact? number) (rational? number)))

(define (rational-coefficient number)
  (unless (exact-rational? number)
    (error "invalid type, expected exact rational number" number))
  number)

(define (rational-sum q1 q2)
  (+ (rational-coefficient q1) (rational-coefficient q2)))

(define (rational-product q1 q2)
  (* (rational-coefficient q1) (rational-coefficient q2)))

(define (rational-zero? q)
  (zero? (rational-coefficient q)))

(define (rational-inverse q)
  (/ (rational-coefficient q)))

(define rational-field
  (make-field rational-coefficient
	      exact-rational?
	      rational-coefficient
	      rational-sum
	      rational-product
	      rational-zero?
	      rational-inverse))
