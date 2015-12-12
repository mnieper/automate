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

(define-algebra b (rational-field grevlex-ordering
				  (x y z)
				  ((+ (4 z) (-4 x y 2) (-16 x 2) -1)
				   (+ (2 y 2 z) (4 x) 1)
				   (+ (2 x 2 z) (2 y 2) x))))

(test-eqv 0 (b (+ (4 x y 4) (16 x 2 y 2) (y 2) (8 x) 2)))

(test-end "automate")
