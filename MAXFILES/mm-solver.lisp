(if (not (find-package :mm-mdj)) (make-package :mm-mdj))
(in-package :mm-mdj)
;; mm.lisp should already have been loaded!
(use-package :mm)

;;=======================================================================
;; File: mm-solver.lisp
;; Infrastructure for Mastermind course project, CMSC 471, Fall 2011
;; (c) Marie desJardins, October 2011
;; VERSION 1.0:  LAST UPDATED 10/4/11
;;
;; Code to define a single simple solver for Mastermind
;;
;; Global variable: *mm-gentest-player* (a player instance)
;;
;; Functions:
;;   - mm-gentest() - naive generate-and-test guesser (so naive
;;     that it utterly ignores the white and red responses)
;;   - mm-next-guess-lex() - return the next guess lexicographically
;;
;; To run a simple test:
(defun test()
  (mm-tournament :colors '(mm::r mm::b mm::y) 
		 :code-length 3
		 :gen-fn 'mm-gen-random
		 :players (list *mm-gentest-player*) 
		 :rounds 2))
(defun number-to-guess (number)
	(let ((temp-cfg '()))
		(loop for i from 0 to (- (length number) 1) do
			(setf temp-cfg (append temp-cfg (list (nth (nth i number) *colors*)) ))		
		)
		temp-cfg
	)
)

;;=======================================================================
;; GLOBAL VARIABLE

;; Global variable that stores a player instance for
;; the generate-and-test player
(defvar *mm-gentest-player*)
(defvar *color-pool*)
(defvar *cfg*)
(defvar *pre-guesses*)
(defvar *prev-considered*)
(setf *prev-considered (make-hash-table :test #'equal))
(setf *color-pool*
	(let ((temp-color-pool '()))
		(loop for i from 0 to (- (length *colors*) 1) do
			(setf temp-color-pool (append temp-color-pool (list (list i 100)) ))
		)
		temp-color-pool
	)
)
(defun sum-prob ()
	(let ((result 0))
		(loop for i from 0 to (- (length *color-pool*) 1) do
			(setf result (+ result (car (cdr (nth i *color-pool* ))) ))
		)
		result
	)

)
(defun prob-random-color (sum-of-prob)
	(let ((random-index (random sum-of-prob)) )
		(loop for i from 0 to (- (length *color-pool*) 1) do
			(if (< random-index (car (cdr (nth i *color-pool* ))))
				(return-from prob-random-color i)
			)
			(setf random-index (- random-index (car (cdr (nth i *color-pool* )))))
		)
	)
)
(defun generate-cfg ()
	(let ((temp-cfg '()) (sum-of-prob (sum-prob)))
		(cond ((eq sum-of-prob 0)
			(loop for i from 1 to *code-length* do
				(setf temp-cfg (append temp-cfg (list (random (- (length *color-pool*) 1))) ))
			)
			temp-cfg
		      )
		     (t
			(loop for i from 1 to *code-length* do
				(setf temp-cfg (append temp-cfg (list (prob-random-color (sum-prob))) ))
			)
			temp-cfg
		      )
		)
	)
)
(setf *cfg*
	(generate-cfg)
)
(setf *prev-guesses* 
	(let () 
		(multiple-value-setq (red white) (mm-score (number-to-guess *cfg*)))
		(list (list *cfg* (list red white)))
	)
)
(setf *mm-gentest-player*
      (make-instance 'mm::player
		     :name 'mm-gentest
		     :guess-fn 'mm-gentest))



;;Test stuff:


(defun test-my()
	(print *color-pool*)
	nil
)


;;=======================================================================
;; MAIN FUNCTIONS

;; You should use this as a template for how to design a player.
;;
;; Your guesser should take three arguments:  a list of colors,
;; a code length, and a generator function (i.e., the reference
;; to the function that was used to generate the codes)
;;
;; Your guesser should call mm-score() to score each of your guesses.  
;; (mm-score() will keep track ;; of the number of guesses.) 
;;  The return value of the guesser is ignored by the tournament function.

;(defun mm-gentest (&optional (colors *colors*) (code-length *code-length*)
;			     (generator #'mm-gen-random)
;			     &aux reds whites)
;  "Super 1337 guesser"
;  (declare (ignore generator))
;  (let ((guess (loop for i from 1 to code-length collect (car colors))))
;    (loop while guess
;	  do (progn
;	       (format t "Guessing ~s...~%" guess)
;	       (multiple-value-setq (reds whites)
;				    (mm-score guess *code* colors))
;	       (cond ((eq reds code-length)
;		      (format t "mm-gentest guessed ~s correctly in ~s guesses!~%"
;			      guess *guesses*)
;		      (return-from mm-gentest))
;		     (t (setf guess (next-guess-lex guess colors
;						    code-length)))))))
 ; )

(defun mm-gentest (&optional (colors *colors*) (code-length *code-length*)
			     (generator #'mm-gen-random)
			     &aux reds whites)
  "Super 1337 guesser"
	(format t "Guessing ~s...~%" *cfg*)
	(setf reds (car (car (cdr (car (last *prev-guesses*))))))
	(setf whites (car (cdr (car (cdr (car (last *prev-guesses*)))))))
 
	(let ((guess *cfg*) (red 0) (white 0))
		(loop while (not (eq red *code-length*)) do
			(setf guess (generate-valid-guess  (car (car (cdr (car (last *prev-guesses*))))) (car (cdr (car (cdr (car (last *prev-guesses*))))))))
			(format t "Guessing ~s...~%" (number-to-guess guess))
			(multiple-value-setq (red white) (mm-score (number-to-guess guess)))
			(setf *prev-guesses* (cons *prev-guesses* (list guess (list red white))))
			(if (> (basic-heuristic red white) (basic-heuristic reds whites))
				(setf *cfg* guess)
			)
			(setf reds red)
			(setf whites white)
		)
		(format t "mm-gentest guessed ~s correctly in ~s guesses!~%"
			      guess *guesses*)
	)
)
(defun generate-valid-guess (red white)
	*cfg*
)
(defun basic-heuristic (red white)
	(+ red white)
)
;; A helper function for mm-gentest that returns the next
;; guess lexicographically (nil when there are no more guesses),
;; given a current guess, list of colors, and code length.
(defun next-guess-lex (guess colors code-length)
  "'Add one' to GUESS lexicographically; return nil if no more.  
Does not modify original guess"
  ;; Create a new copy of the guess to overwrite
  (setf guess (copy-list guess))

  ;; "Add one" to the first color, "carrying" to the next column
  ;; as necessary.  If the last index is already at the highest
  ;; color, then no more guesses -> set guess to nil
  (let ((last-color (car (last colors))))
    (loop for i from 0 to (- code-length 1)
	  do (let ((cur-color (nth i guess)))
	       (cond ((eq cur-color last-color)
		      (if (eq i (- code-length 1))
			  ;; No more "columns" to "carry" -> no more guesses
			  (return-from next-guess-lex nil)
			;; "Carry" this column -> first color
			(setf (nth i guess) (first colors))))
		     (t 
		      ;; More colors -> increment this color and return
		      (setf (nth i guess) (cadr (member cur-color colors)))
		      (return-from next-guess-lex guess)))))
    nil))


(defun list-to-string (a)
	(setf new-string "")
 	(loop for x from 0 to (- (length a) 1) do
                    (setf new-string (concatenate
                               'string new-string
                               (string (write-to-string (nth x a))))))
 new-string)

(defun is-valid-guess (guess)
	(loop for i from 0 to (- (length *prev-guesses*) 1) do
		(multiple-value-setq (red white) (mm-score (number-to-guess guess) (number-to-guess (car (nth 0 *prev-guesses*))) *colors* t))
		;(print red)
		;(print (car (car (cdr (nth i *prev-guesses*)))))
		;(print (not (eq red (car (car (cdr (nth i *prev-guesses*)))))))

		;(print white)
		;(print (car (cdr (car (cdr (nth i *prev-guesses*))))))
		;(print (not (eq white (car (cdr (car (cdr (nth i *prev-guesses*))))))))

		(if (or (not (eq red (car (car (cdr (nth i *prev-guesses*)))))) (not (eq white (car (cdr (car (cdr (nth i *prev-guesses*))))))) ) 
			(return-from is-valid-guess nil)
		)
	)
	t
)

