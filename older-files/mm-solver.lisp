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


;;=======================================================================
;; GLOBAL VARIABLE

;; Global variable that stores a player instance for
;; the generate-and-test player
(defvar *mm-gentest-player*)
(setf *mm-gentest-player*
      (make-instance 'mm::player
		     :name 'mm-gentest
		     :guess-fn 'mm-gentest))


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

(defun mm-gentest (&optional (colors *colors*) (code-length *code-length*)
			     (generator #'mm-gen-random)
			     &aux reds whites)
  "Stupid generate-and-test guesser that doesn't pay any attention to the whites & reds"
  (declare (ignore generator))
  (let ((guess (loop for i from 1 to code-length collect (car colors))))
    (loop while guess
	  do (progn
	       (format t "Guessing ~s...~%" guess)
	       (multiple-value-setq (reds whites)
				    (mm-score guess *code* colors))
	       (cond ((eq reds code-length)
		      (format t "mm-gentest guessed ~s correctly in ~s guesses!~%"
			      guess *guesses*)
		      (return-from mm-gentest))
		     (t (setf guess (next-guess-lex guess colors
						    code-length)))))))
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
