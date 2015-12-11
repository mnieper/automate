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

(define-syntax define-algebra
  (syntax-rules ()
    ((define-algebra algebra (fld order (x ...) (f ...)))
     (begin
       (define field fld)
       (define ordering order)
       (define indeterminates #(x ...))
       (define ideal
	 (groebner-basis field
			 ordering
			 ((eval `(lambda (field ordering)
				   ,(compile-ideal indeterminates `(f ...)))
				algebra-environment)
		      field ordering)))
       (define-syntax algebra
	 (syntax-rules ()
	   ((algebra code)
	    (externalize-polynomial field
				    ordering
				    indeterminates
				    (reduce field
					    ordering
					    ideal
					    ((eval `(lambda (field ordering)
						      ,(compile indeterminates `code))
						   algebra-environment)
					     field ordering))))))))))

(define (compile-ideal indeterminates polynomials)
  `(list ,@(map (lambda (polynomial)
		  (compile indeterminates polynomial))
		polynomials)))

(define (compile indeterminates code)
  (cond
   ((exact-rational? code)
    `(make-polynomial field
		      ordering
		      (list (make-term (make-coefficient field ,code)
				       (make-monomial ordering
						      ,(make-vector (vector-length indeterminates)
								    0))))))
   ((pair? code)
    (case (car code)
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

(define (compile-sum summands)
  (cond
   ((null? summands)
    `(make-polynomial field ordering (list)))
   ((null? (cdr summands))
    (car summands))
   (else
    `(polynomial-sum field ordering ,(car summands) ,(compile-sum (cdr summands))))))

(define (compile-product summands)
  (cond
   ((null? summands)
    `(make-polynomial field ordering (list)))
   ((null? (cdr summands))
    (car summands))
   (else
    `(polynomial-product field ordering ,(car summands) ,(compile-product (cdr summands))))))

(define (degree number)
  (or (>= number 0)
      (error "invalid exponent" number)))

(define (compile-monomial indeterminates code)
  (let*-values
      (((number code) (if (exact-rational? (car code))
			  (values (car code) (cdr code))
			  (values 1 code)))
       ((n) (vector-length indeterminates))
       ((exponent) (make-vector n 0)))    
    (let loop1 ((code code))
      (if (null? code)
	  `(make-polynomial field ordering (list (make-term (make-coefficient field ,number)
							    (make-monomial ordering ,exponent))))
	  (let ((indeterminate (car code)))
	    (let loop2 ((i 0))
	      (cond
	       ((= i n) (error "unknown indeterminate" indeterminate))
	       ((eq? indeterminate (vector-ref indeterminates i))
		(let-values
		    (((degree code) (if (and (not (null? (cdr code)))
					     (exact-integer? (cadr code))
					     (degree (cadr code)))
					(values (cadr code) (cddr code))
					(values 1 (cdr code)))))
		  (vector-set! exponent i (+ (vector-ref exponent i) degree))
		  (loop1 code)))
	       (else (loop2 (+ i 1))))))))))
