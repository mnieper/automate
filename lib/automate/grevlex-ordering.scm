(define (compute-total-degree exponent)
  (let ((n (vector-length exponent)))
    (do ((i 0 (+ i 1)) (d 0 (+ d (vector-ref exponent i))))
	((= i n) d))))

(define-record-type grevlex-monomial-type
  (%grevlex-monomial degree exponent)
  grevlex-monomial?
  (degree total-degree)
  (exponent grevlex-exponent))

(define (grevlex-monomial exponent)
  (%grevlex-monomial (compute-total-degree exponent) exponent))

(define (grevlex>? monomial1 monomial2)
  (let ((d1 (total-degree monomial1)) (d2 (total-degree monomial2)))
    (or (> d1 d2)
	(and (= d1 d2)
	     (let ((e1 (grevlex-exponent monomial1)) (e2 (grevlex-exponent monomial2)))
	       (let loop ((n (vector-length e1)))
		 (and (> n 0)
		      (let* ((i (- n 1)) (n1 (vector-ref e1 i)) (n2 (vector-ref e2 i)))
			(or (< n1 n2)
			    (and (= n1 n2)
				 (loop i)))))))))))

(define (grevlex-product monomial1 monomial2)
  (let* ((e1 (grevlex-exponent monomial1)) (e2 (grevlex-exponent monomial2))
	 (n (vector-length e1))
	 (e (make-vector n)))
    (do ((i 0 (+ i 1)))
	((= i n))
      (vector-set! e i (+ (vector-ref e1 i) (vector-ref e2 i))))
    (%grevlex-monomial (+ (total-degree monomial1) (total-degree monomial2)) e)))

(define (grevlex-quotient monomial1 monomial2)
  (let ((d1 (total-degree monomial1)) (d2 (total-degree monomial2)))
    (and (>= d1 d2)
	 (let* ((e1 (grevlex-exponent monomial1)) (e2 (grevlex-exponent monomial2))
		(n (vector-length e1))
		(e (make-vector n)))
	   (let loop ((i 0))
	     (if (= i n)
		 (%grevlex-monomial (- d1 d2) e)
		 (let ((d (- (vector-ref e1 i) (vector-ref e2 i))))
		   (and (>= d 0)
			(begin
			  (vector-set! e i d)
			  (loop (+ i 1)))))))))))

(define (grevlex-syzygy monomial1 monomial2)
  (let*
      ((e1 (grevlex-exponent monomial1)) (e2 (grevlex-exponent monomial2))
       (n (vector-length e1))
       (r1 (make-vector n 0)) (r2 (make-vector n 0)))
    (let loop ((i 0) (d 0))
      (if (= i n)
	  (values (%grevlex-monomial (- d (total-degree monomial1)) r1)
		  (%grevlex-monomial (- d (total-degree monomial2)) r2))
	  (let ((d1 (vector-ref e1 i)) (d2 (vector-ref e2 i)))
	    (cond
	     ((> d1 d2)
	      (vector-set! r2 i (- d1 d2))
	      (loop (+ i 1) (+ d (- d1 d2))))
	     ((< d1 d2)
	      (vector-set! r1 i (- d2 d1))
	      (loop (+ i 1) (+ d (- d2 d1))))
	     (else (loop (+ i 1) d))))))))

(define grevlex-ordering
  (make-ordering grevlex-monomial
		 grevlex-monomial?
		 grevlex>?
		 grevlex-exponent
		 grevlex-product
		 grevlex-quotient
		 grevlex-syzygy))
