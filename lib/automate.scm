;;; automate.scm --- Automate user interface

;; Copyright (C) 2015 Marc Nieper-Wißkirchen

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define algebra-environment (environment '(scheme base)
					 '(automate coefficient)
					 '(automate monomial)
					 '(automate polynomial)))

(define d 'd)

(define-syntax define-algebra
  (syntax-rules ()
    ((define-algebra A (field ordering (x ...) (f ...)))
     (begin
       (define indeterminates `#(x ...))
       (define algebra (make-differential-algebra field ordering (vector-length indeterminates)))
       (define ideal
	 (groebner-basis algebra
			 ((eval `(lambda (algebra)
				   ,(compile-ideal indeterminates `(f ...)))
				algebra-environment)
		      algebra)))
       (define-syntax A
	 (syntax-rules ()
	   ((A code)
	    (externalize-differential-form
	     algebra
	     indeterminates
	     (reduce algebra ; FIXME reduce-differential-form to add d(ideal).
		     ideal
		     ((eval `(lambda (algebra)
			       ,(compile indeterminates `code))
			    algebra-environment)
		      algebra))))))))))

(define (compile-ideal indeterminates polynomials)
  `(list ,@(map (lambda (polynomial)
		  (compile indeterminates polynomial))
		polynomials)))

(define (compile indeterminates code)
  (cond
   ((exact-rational? code)
    `(make-polynomial algebra
		      (list (make-term (make-coefficient (field algebra) ,code)
				       (make-monomial (ordering algebra)
						      ,(make-vector (vector-length indeterminates)
								    0))))))
   ((pair? code)
    (case (car code)
      ((d) (compile-derivative indeterminates (cdr code)))
      ((+) (compile-sum (compile* indeterminates (cdr code))))
      ((*) (compile-product (compile* indeterminates (cdr code))))
      (else (compile-monomial indeterminates code))))
   ((symbol? code)
    (compile-monomial indeterminates (list code)))
   (else (error "invalid code" code))))

(define (compile* indeterminates code*)
  (map (lambda (code)
	 (compile indeterminates code))
       code*))

(define (compile-derivative indeterminates code)
  (let ((n (vector-length indeterminates)))
    `(let ((polynomial ,(compile indeterminates (car code))))
       (let loop ((i 0) (derivative zero-polynomial))
	 (if (= i ,n)
	     derivative
	     (let ((e (make-vector ,(* 2 n) 0)))
	       (vector-set! e (+ i ,n) 1)
	       (loop (+ i 1)
		     (polynomial-sum algebra
				     derivative
				     (polynomial-product
				      algebra
				      (make-polynomial
				       algebra
				       (list
					(make-term (make-coefficient (field algebra) 1)
						   (make-monomial (ordering algebra) e))))
				      (polynomial-derivative algebra polynomial i))))))))))

(define (compile-sum summands)
  (cond
   ((null? summands)
    `(make-polynomial algebra (list)))
   ((null? (cdr summands))
    (car summands))
   (else
    `(polynomial-sum algebra ,(car summands) ,(compile-sum (cdr summands))))))

(define (compile-product summands)
  (cond
   ((null? summands)
    `(make-polynomial algebra (list)))
   ((null? (cdr summands))
    (car summands))
   (else
    `(polynomial-product algebra ,(car summands) ,(compile-product (cdr summands))))))

(define (degree number)
  (or (>= number 0)
      (error "invalid exponent" number)))

(define (compile-monomial indeterminates code)
  (let*-values
      (((number code) (if (exact-rational? (car code))
			  (values (car code) (cdr code))
			  (values 1 code)))
       ((exponent) (make-vector (* 2 (vector-length indeterminates)) 0)))
    (let loop ((code code))
      (if (null? code)
	  `(make-polynomial algebra (list (make-term (make-coefficient (field algebra) ,number)
						     (make-monomial (ordering algebra) ,exponent))))
	  (let ((i (find-indeterminate indeterminates (car code))))
	    (unless i (error "unknown indeterminate" (car code)))
	    (let-values
		(((degree code) (if (and (not (null? (cdr code)))
					 (exact-integer? (cadr code))
					 (degree (cadr code)))
				    (values (cadr code) (cddr code))
				    (values 1 (cdr code)))))
	      (vector-set! exponent i (+ (vector-ref exponent i) degree))
	      (loop code)))))))

(define (find-indeterminate indeterminates code)
  (let ((n (vector-length indeterminates)))
    (let loop ((i 0))
      (and (< i n)
	   (if (eq? code (vector-ref indeterminates i))
	       i
	       (loop (+ i 1)))))))
