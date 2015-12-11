(import (scheme base)
	(automate rational-field)
	(automate grevlex-ordering)
	(automate))

(test-begin "automate")

(define-algebra a (rational-field grevlex-ordering
				  (x y)
				  ((+ (x 3) (-2 x y))
				   (+ (x 2 y) (-2 y 2) x))))

(test-eqv 0 (a (x 2)))

(test-end "automate")
