;;; repl.scm --- Automate REPL

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

(import (scheme base)
	(scheme read)
	(scheme write)
	(automate error)
	(automate eval))

(define *notice* "\
Automate version 0.0.1, Copyright (C) 2015 Marc Nieper-Wißkirchen
This program comes with ABSOLUTELY NO WARRANTY; for details type `(warranty)'.
This is free software, and you are welcome to redistribute it
under certain conditions; type `(copying)' for details.
")

(write-string *notice*)

(define (display-error error-object)	       
  (write-string "ERROR: ")
  (write-string (automate-error-object-message error-object))
  (let ((irritants (automate-error-object-irritants error-object)))
    (unless (null? irritants)
      (write-string ": ")
      (display (car irritants))))
  (newline))

(define (loop return)
  (let ((environment (make-automate-environment return)))
    (let loop ()
      (display (current-label environment))
      (write-string "> ")
      (let ((code (read)))
	(guard (error-object
		((automate-error-object? error-object)
		 (display-error error-object)))
	       (display (automate-eval environment code))     
	     (newline)))
      (loop))))

(call-with-current-continuation loop)
