(define-record-type algebra-type
  (make-algebra field ordering odds)
  algebra?
  (field field)
  (ordering ordering)
  (odds odds))

(define-record-type term-type
  (make-term coefficient monomial)
  term?
  (coefficient term-coefficient)
  (monomial term-monomial))

(define-record-type polynomial-type
  (%make-polynomial terms)
  polynomial?
  (terms terms))

(define (term-compare algebra term1 term2)
  (let ((ordering (ordering algebra))
	(monomial1 (term-monomial term1))
	(monomial2 (term-monomial term2)))
    (cond
     ((monomial>? ordering monomial1 monomial2) 1)
     ((monomial>? ordering monomial2 monomial1) -1)
     (else 0))))

(define (term-merge algebra term1 term2)
  (let ((field (field algebra)))
    (let ((coefficient (coefficient-sum field (term-coefficient term1) (term-coefficient term2))))
      (and (not (coefficient-zero? field coefficient))
	   (make-term coefficient (term-monomial term1))))))

(define (term-product algebra term1 term2)
  (let
      ((field (field algebra))
       (ordering (ordering algebra))
       (odds (odds algebra)))
    (make-term (coefficient-product field
				    (term-coefficient term1)
				    (term-coefficient term2))
	       (monomial-product ordering odds
				 (term-monomial term1)
				 (term-monomial term2)))))

(define (runs algebra terms)
  (let loop ((terms terms))
    (if (null? terms)
	'()
	(let ((term1 (car terms)) (runs (loop (cdr terms))))
	  (if (null? runs)
	      (list (list term1))
	      (let* ((run (car runs)) (term2 (car run)))
		(case (term-compare algebra term1 term2)
		  ((1) (cons (cons term1 run) (cdr runs)))
		  ((-1) (cons (list term1) runs))
		  (else
		   (let ((term (term-merge algebra term1 term2)))
		     (cond
		      (term (cons (cons term (cdr run)) (cdr runs)))
		      ((null? (cdr run)) (cdr runs))
		      (else (cons (cdr run) (cdr runs)))))))))))))

(define (terms-merge algebra run1 run2)
  (let loop ((run1 run1) (run2 run2))
    (cond
     ((null? run1) run2)
     ((null? run2) run1)
     (else
      (let ((term1 (car run1)) (term2 (car run2)))
	(case (term-compare algebra term1 term2)
	  ((1) (cons term1 (loop (cdr run1) run2)))
	  ((-1) (cons term2 (loop run1 (cdr run2))))
	  (else
	   (let ((term (term-merge algebra term1 term2)))
	     (if term
		 (cons term (loop (cdr run1) (cdr run2)))
		 (loop (cdr run1) (cdr run2)))))))))))
      
(define (make-polynomial algebra terms)
  (%make-polynomial
   (let loop ((runs (runs algebra terms)))
     (cond
      ((null? runs) '())
      ((null? (cdr runs)) (car runs))
      (else
       (loop
	(let loop ((runs runs))
	  (cond
	   ((null? runs) '())
	   ((null? (cdr runs)) runs)
	   (else
	    (let ((run (terms-merge algebra (car runs) (cadr runs))))
	      (if (null? run)
		  (loop (cddr runs))
		  (cons run (loop (cddr runs))))))))))))))

(define (make-homogeneous-polynomial coefficient monomial)
  (%make-polynomial (list (make-term coefficient monomial))))

(define zero-polynomial (%make-polynomial '()))

(define (polynomial-zero? polynomial)
  (null? (terms polynomial)))

(define (leading-term polynomial)
  (car (terms polynomial)))

(define (polynomial-remainder polynomial)
  (cdr (terms polynomial)))

(define (polynomial-sum algebra polynomial1 polynomial2)
  (make-polynomial algebra (append (terms polynomial1) (terms polynomial2))))

(define (polynomial-weighted-difference algebra c1 m1 p1 c2 m2 p2)
  (let*
      ((field (field algebra))
       (c2 (coefficient-negative field c2))
       (m1 (make-homogeneous-polynomial c1 m1))
       (m2 (make-homogeneous-polynomial c2 m2))
       (p1 (polynomial-product algebra m1 p1))
       (p2 (polynomial-product algebra m2 p2)))
    (polynomial-sum algebra p1 p2)))

(define (polynomial-product algebra polynomial1 polynomial2)
  (make-polynomial algebra
		   (let loop1 ((terms1 (terms polynomial1)) (terms2 (terms polynomial2)))
		     (if (null? terms1)
			 '()
			 (let ((term (car terms1)))
			   (let loop2 ((terms terms2))
			     (if (null? terms)
				 (loop1 (cdr terms1) terms2)
				 (cons (term-product algebra term (car terms))
				       (loop2 (cdr terms))))))))))

(define (externalize-term algebra indeterminates term)
  (let* ((field (field algebra))
	 (ordering (ordering algebra))
	 (coefficient (externalize-coefficient field (term-coefficient term)))
	 (monomial (externalize-monomial ordering indeterminates (term-monomial term))))
    (cond
     ((eq? monomial 1) coefficient)
     ((eq? coefficient 1) monomial)
     ((pair? monomial) (cons coefficient monomial))
     (else (list coefficient monomial)))))

(define (externalize-polynomial algebra indeterminates polynomial)
  (let ((polynomial (map (lambda (term)
			   (externalize-term algebra indeterminates term))
			 (terms polynomial))))
    (cond
     ((null? polynomial) 0)
     ((null? (cdr polynomial)) (car polynomial))
     (else (cons '+ polynomial)))))

(define (polynomial-division algebra polynomial divisor)
  (let*
      ((field (field algebra))
       (ordering (ordering algebra))
       (lt1 (leading-term polynomial))
       (lt2 (leading-term divisor))
       (mq (monomial-quotient ordering (term-monomial lt1) (term-monomial lt2))))
    (and mq
	 (polynomial-sum algebra
			 polynomial
			 (polynomial-product algebra
					     (%make-polynomial (list (make-term
								      (coefficient-negative-quotient
								       field
								       (term-coefficient lt1)
								       (term-coefficient lt2))
								      mq)))
					     divisor)))))

(define (reduce algebra ideal polynomial)
  (let loop1 ((polynomial polynomial) (remainder zero-polynomial))
    (if (polynomial-zero? polynomial)
	remainder
	(let ((lt (leading-term polynomial)))
	  (let loop2 ((ideal ideal))
	    (if (null? ideal)
		(loop1 (%make-polynomial (polynomial-remainder polynomial))
		       (polynomial-sum algebra remainder
				       (%make-polynomial (list lt))))
		(let ((polynomial (polynomial-division algebra polynomial (car ideal))))
		  (if polynomial
		      (loop1 polynomial remainder)
		      (loop2 (cdr ideal))))))))))

(define (syzygy-polynomial algebra p1 p2)
  (let ((field (field algebra)) (ordering (ordering algebra)))
    (let*-values
	(((lt1) (leading-term p1)) ((lt2) (leading-term p2))
	 ((m1 m2) (monomial-syzygy ordering (term-monomial lt1) (term-monomial lt2)))
	 ((c1) (coefficient-inverse field (term-coefficient lt1)))
	 ((c2) (coefficient-inverse field (term-coefficient lt2))))
      (polynomial-weighted-difference algebra c1 m1 p1 c2 m2 p2))))

(define (reduced-syzygy-polynomial algebra ideal p1 p2)
  (reduce algebra ideal (syzygy-polynomial algebra p1 p2)))

(define (reduce-ideal ideal)
  (let loop ((ideal ideal))
    (cond
     ((null? ideal) '())
     ((polynomial-zero? (car ideal)) (loop (cdr ideal)))
     (else (cons (car ideal) (loop (cdr ideal)))))))

(define (groebner-basis algebra ideal)
  (let loop ((ideal (reduce-ideal ideal)))
    (let loop1 ((ideal1 ideal))
      (if (null? ideal1)
	  ideal
	  (let loop2 ((ideal2 (cdr ideal1)))
	    (if (null? ideal2)
		(loop1 (cdr ideal1))
		(let ((polynomial (reduced-syzygy-polynomial algebra
							     ideal
							     (car ideal1)
							     (car ideal2))))
		  (if (polynomial-zero? polynomial)
		      (loop2 (cdr ideal2))
		      (loop (cons polynomial ideal))))))))))
