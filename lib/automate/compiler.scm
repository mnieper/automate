(define (compile code)
  `(lambda (environment)
     ,(%compile code)))

(define (compile-procedure args name compiler argc)
  (unless (or (< argc 0) (= (length args) argc))
    (automate-error "wrong number of arguments" (cons name args)))
  (apply compiler args))

(define (compile-warranty) "\
Automate 0.0.1
Copyright 2015 Marc Nieper-Wißkirchen

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
")

(define (compile-copying) "\
Automate 0.0.1
Copyright 2015 Marc Nieper-Wißkirchen

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.
")

(define (compile-exit)
  `((return environment)))

(define (compile-label label)
  (unless (symbol? label)
    (automate-error "invalid label, expected symbol" label))
  `(begin
     (set-label! environment ',label)
     "Label set"))

(define (compile-goto label)
  `(begin
     (state-pop! environment ',label)
     "Gone."))

(define (compile-modulo p)
  (unless (and (exact-integer? p) (prime? p))
	  (automate-error "invalid characteristic, prime expected" p))
  `(begin
     (set-characteristic! environment ,p)
     "Characteristic set."))

(define (compile-global . vars)
  (for-each (lambda (var)
	      (unless (symbol? var) (automate-error "invalid variable, symbol expected" var)))
	    vars)
  `(begin
     ,@(map (lambda (var)
	      `(add-global! environment ',var))
	    vars)
     "Global variables introduced."))
  
(define (compile-local . vars)
  (for-each (lambda (var)
	      (unless (symbol? var) (automate-error "invalid variable, symbol expected" var)))
	    vars)
  `(begin
     ,@(map (lambda (var)
	      `(add-local! environment ',var))
	    vars)
     "Local variables introduced."))

(define (compile-reference var)
  `(variable environment ,var))

(define procedures
  `((warranty ,compile-warranty 0)
    (copying ,compile-copying 0)
    (exit ,compile-exit 0)
    (label ,compile-label 1)
    (goto ,compile-goto 1)
    (modulo ,compile-modulo 1)
    (global ,compile-global -1)
    (local ,compile-local -1)))

(define (%compile code)
  (cond
   ((pair? code)
    (cond
     ((assq (car code) procedures) => (lambda (procedure)
					(apply compile-procedure (cdr code) procedure)))
     (else (automate-error "unknown procedure" (car code)))))
   ((symbol? code)
    (compile-reference code))
   (else code)))