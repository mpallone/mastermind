;; Max Spector and Mark Pallone
;; mspecto1@umbc.edu         markpa1@umbc.edu
;; 
;; CMSC 471 Final Submission
;; 11/17/2011
;; 



(if (not (find-package :mm-markpa1)) (make-package :mm-markpa1))
(in-package :mm-markpa1)
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
  (mm-tournament :colors *colors*
		 :code-length *code-length*
		 :gen-fn 'mm-gen-random
		 :players (list *mm-gentest-player*) 
		 :rounds 5))

;; Function to convert our numerical color representation into the color representation of mm.lisp
(defun number-to-guess (number)
	(let ((temp-cfg '()))
		(loop for i from 0 to (- (length number) 1) do
			(setf temp-cfg (append temp-cfg (list (nth (nth i number) *mm-colors*)) ))		
		)
		temp-cfg
	)
)


;;=======================================================================
;; GLOBAL VARIABLES

(defvar *num-considered-guesses*) ;; how many guesses were considered in total, including ones that weren't "offic\ially" guessed?
(defvar *num-guesses-evaluated*) ;; how many guesses are sent to isValidGuess?
(defvar *high-punish-score*) ;; that max score a guess can still receive, and still have its probabilities reduced.
(defvar *bad-guesses*) ;; The list of anti hill climbing guesses to punish
(defvar *mm-gentest-player*) ;; The player 
(defvar *color-pool*) ;;The pool of color numbers and their probabilities
(defvar *cfg*) ;;The Current Favorite Guess
(defvar *pre-guesses*) ;;The list of previous guesses that we submitted an their red white values
(defvar *prev-heuristic*) ;;The previous heuristic value, used to detect anti-hill climbing
(defvar *prev-considered*) ;;The hash table of previously considered guesses
(defvar *mm-colors*) ;;The global variable of the color list
(defvar *mm-code-length*) ;;The global variable of the size of the code
(defvar *superconstant*) ;;The number of guesses to compare to the prev-guesses before submiting the best

;;Setting some things to 0
(setf *bad-guesses* '())
(setf *prev-heuristic* 0)

;;Returns the sum of all of the color probabiliities, used as prob-random-color's helper function
(defun sum-prob ()
	(let ((result 0))
		(loop for i from 0 to (- (length *color-pool*) 1) do
			(setf result (+ result (car (cdr (nth i *color-pool* ))) ))
		)
		result
	)

)

;;Generates a random color based on the color-pool probability of landing on that color
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

;;Generated the current favorite guess
(defun generate-cfg ()
	(let ((temp-cfg '()) (sum-of-prob (sum-prob)))
		(cond ((eq sum-of-prob 0)
			(loop for i from 1 to *mm-code-length* do
				(setf temp-cfg (append temp-cfg (list (random (- (length *color-pool*) 1))) ))
			)
			temp-cfg
		      )
		     (t
			(loop for i from 1 to *mm-code-length* do
				(setf temp-cfg (append temp-cfg (list (prob-random-color (sum-prob))) ))
			)
			temp-cfg
		      )
		)
	)
)

;;Resets the color pool to 0 probabilities for all colors
(defun reset-color-pool ()
	(setf *color-pool*
		(let ((temp-color-pool '()))
			(loop for i from 0 to (- (length *mm-colors*) 1) do
				(setf temp-color-pool (append temp-color-pool (list (list i 100)) ))
			)
			temp-color-pool
		)
	)
)

;;Adds the *cfg* to teh prev-guesses table
(defun add-cfg-to-prev ()
	(setf *prev-guesses* 
		(let () 
			(multiple-value-setq (red white) (mm-score (number-to-guess *cfg*) *code* *mm-colors*))
			(list (list *cfg* (list red white)))
		)
	)
)

;;Creates the mastermind player
(setf *mm-gentest-player*
      (make-instance 'mm::player
		     :name 'mm-gentest
		     :guess-fn 'mm-gentest))



;;=======================================================================
;; MAIN FUNCTIONS

;;The main function, it sets the super-constant, adds bad guess to teh bad-guess list, calls generate-valid-guess,
;; and sets the *cfg* to the new guess if it is better.
(defun mm-gentest (&optional (colors *colors*) (code-length *code-length*)
			     (generator #'mm-gen-random)
			     &aux reds whites)
	(setf *mm-colors* colors)

	(setf *mm-code-length* code-length)
	(reset-color-pool)
	(setf *prev-considered* (make-hash-table :test #'equal))
	(setf *cfg* (generate-cfg))
	(setf *bad-guesses* '())
	(setf *prev-heuristic* 0)

	;; statistical stuff
	(setf *num-considered-guesses* 0)
	(setf *num-guesses-evaluated* 0)
	(if (>= *mm-code-length* 11) (setf *superconstant* 200))
	(if (and (< *mm-code-length* 11) (>= *mm-code-length* 8)) (setf *superconstant* 2200))
	(if (and (< *mm-code-length* 8) (>= *mm-code-length* 6)) (setf *superconstant* 1900))
	(if (and (< *mm-code-length* 6) (>= *mm-code-length* 5)) (setf *superconstant* 1400))
	(if  (< *mm-code-length* 5) (setf *superconstant* 1000))
	
	
	;; set the punishable score to be 25% of the highest possible score
	(setq maximum-score (basic-heuristic code-length 0))
	(setq *high-punish-score* (* 0 maximum-score))

	(add-cfg-to-prev)	
	(setf reds (car (car (cdr (car (last *prev-guesses*))))))
	(setf whites (car (cdr (car (cdr (car (last *prev-guesses*)))))))
 
	(let ((guess *cfg*) (red 0) (white 0) (time (get-universal-time)))
		(loop while (not (eq red *mm-code-length*)) do
			(setf guess (generate-valid-guess reds whites))
			(multiple-value-setq (red white) (mm-score (number-to-guess guess) *code* *mm-colors*))
			(setf *prev-guesses* (cons (list guess (list red white)) *prev-guesses*))
			(setq score (basic-heuristic red white))
			(if (< score (*  *prev-heuristic* .75))
					(setf *bad-guesses* (append (list guess) *bad-guesses*))
			)
			(if (> score (*  *prev-heuristic* .80))
				(setq *prev-heuristic* score)
			)
			(if (> score (basic-heuristic reds whites))
				(progn (setf *cfg* guess)
				       (setf reds red)
				       (setf whites white)
				)
			)

			;; if guess scores <= 4, reduce probabilities based on the score
			(if (<= (basic-heuristic red white) *high-punish-score*)

			    (progn
			      (if (eq 0 (basic-heuristic red white))
				  (setq punish-percent 1)
				(setq punish-percent (/ (float (basic-heuristic red white)) *high-punish-score*)))
			      (reduce-list guess punish-percent)))			
			(setf time (get-universal-time))
	       )

	     
	)

)

;; IN - see if item is in myList
(defun in (item myList)
  (loop for i in myList do
	(if (eq item i)
	    (return-from in t)))
  '())

;; LOWER-PROBABILITY - Given a color index and a percentage to lower it by,
;; lower that color's probability.
;; 
;; For example, if red has a probability value of 100 and this function is 
;; passed the value .4, red's new value will be 60.
(defun lower-probability (color-index percentage)
  (setq current (nth 1 (nth color-index *color-pool*)))
  (setq new (* current (- 1 percentage)))
;  (if (eq 0 new)     ;; let's allow 0 probability for now
;      (setq new 1))
  (setf (nth 1 (nth color-index *color-pool*)) new))
	
;; REDUCE-LIST - given a list of colors in color-ppol, reduce the probability
;; of each of those values by the percentage specified
(defun reduce-list (color-list percentage)
  ;; don't reduce a color twice
  (setq already-reduced '())

  (loop for i in color-list do
	(if (not (in i already-reduced)) 
	    (progn 
	    (setq already-reduced (append already-reduced (list i)))
	  (setq index (get-color-index i))
	  (lower-probability index percentage)))))
	  
	 

;; GET-COLOR-INDEX - given a value in *color-pool*, return the index of that 
;; value (or -1 if that index doesn't exist) 
(defun get-color-index (color)
  (loop for i from 0 to (- (length *color-pool*) 1) do
	(if (eq color (nth 0 (nth i *color-pool*)))
	    (return-from get-color-index i)))
  -1)

;; Helper function for hash table
(defun list-to-string (a)
	(setf new-string "")
 	(loop for x from 0 to (- (length a) 1) do
                    (setf new-string (concatenate
                               'string new-string
                               (string (write-to-string (nth x a))))))
 new-string)

;;Returns a list containing the distance and the close to bad value, if distance is 0 then the code is valid
(defun is-valid-guess (guess)
 	(let ((distance 0) (close-bad 0))
		(loop for i from (- (length *prev-guesses*) 1) downto 0 do
			(multiple-value-setq (red white) (mm-score (number-to-guess guess) (number-to-guess (car (nth i *prev-guesses*))) *mm-colors* t))
;weighted		(setf distance (* (+ distance (abs (- (basic-heuristic red white) (basic-heuristic (car (car (cdr (nth i *prev-guesses*)))) (car (cdr (car (cdr (nth i *prev-guesses*))))) )))) (basic-heuristic (car (car (cdr (nth i *prev-guesses*)))) (car (cdr (car (cdr (nth i *prev-guesses*))))) ) ))
			(setf distance (+ distance (abs (- (basic-heuristic red white) (basic-heuristic (car (car (cdr (nth i *prev-guesses*)))) (car (cdr (car (cdr (nth i *prev-guesses*))))) )))) )
		)
		(loop for i from 0 to (- (length *bad-guesses*) 1) do
			(multiple-value-setq (red white) (mm-score (number-to-guess guess) (number-to-guess (nth i *bad-guesses*)) *mm-colors* t))
			(setf close-bad (+ close-bad (basic-heuristic red white)))			
		)
	(return-from is-valid-guess (list distance close-bad))
	)
)



;; BASIC-HEURISTIC: given a number of reds and whites, and the total number of
;; pegs in the code, return a score corresponding the "value" of that number
;; of reds and whites. This heuristic comes from section 3.2 of the 
;; Temporel/Kovacs paper. 
(defun basic-heuristic (reds whites)
  ;; These two error conditions shouldn't happen if Dr. DJ wrote her scoring
  ;; function correctly.

  ;; can't ever have more reds and whites than pegs. Just return 0. 
  (if (< *mm-code-length* (+ reds whites))
      (return-from basic-heuristic 0))
  ;; this is an impossible situation; in this case just return a large number.
  (if (and (eq whites 1) (eq (- *mm-code-length* 1) reds))
      (return-from basic-heuristic (expt reds reds)))

  (setq 
   score
   (+ (/ (+ (expt whites 2) whites) 2)
      (* reds whites) 
      (/ (+ (expt reds 2) reds) 2)
      reds)))


;; RANGE: Given a start and end value, return a list going from the start number
;; going to the ending number. Analogous to range() in python. 
(defun range (start end)
  (loop for i from start below end collect i))


;; Generates a guess that is not currently in the hash table
(defun generate-guess (reds whites)
  (setq guess (generate-crappy-guess reds whites))
  (setf guess-string (list-to-string guess))
  
  ;; if the guess has been seen before, generate a new one until you get a 
  ;; unique one
  (let ((trys 0))
 	(loop while (gethash guess-string *prev-considered*) 
     	
		do

		(setq guess (generate-crappy-guess reds whites))
		(setf guess-string (list-to-string guess))
	;	(read)
	
 	 )
	  (setf (gethash guess-string *prev-considered*) guess-string)
	  (setf *num-considered-guesses* (+ *num-considered-guesses* trys))
  )

  guess)


;;Resets the probability numbers in the color-pool
(defun reset-color-pool-prob ()
	(loop for i from 0 to (- (length *color-pool*) 1)
	do
	(setf (nth 1 (nth i *color-pool*)) 100))
)

;;Change the color-pool probability table based on the Temporel/Kovacs paper.
(defun change-color-pool-prob (red-list white-list)
	;;Set all to 0
	(loop for i from 0 to (- (length *color-pool*) 1)
	do
	(setf (nth 1 (nth i *color-pool*)) 0))

	;;Add 100 for all colors in the *cfg*
	(loop for i from 0 to (- (length *cfg*) 1)
	do
	(setf (nth 1 (nth (nth i *cfg*) *color-pool*)) (+ (nth 1 (nth (nth i *cfg*) *color-pool*)) 100)))
	
	;;Subtract 100-45 for all colors that are in the red list
	(loop for i from 0 to (- (length red-list) 1)
	do
	(setf (nth 1 (nth (nth (nth i red-list) *cfg*) *color-pool*)) (- (nth 1 (nth (nth (nth i red-list) *cfg*) *color-pool*)) 55)))

	;;Subtract 100-45 for all colors in the white list
	(loop for i from 0 to (- (length white-list) 1)
	do
	(setf (nth 1 (nth (nth (nth i white-list) *cfg*) *color-pool*)) (- (nth 1 (nth (nth (nth i white-list) *cfg*) *color-pool*)) 55)))
	
	;;Subtract from 100, set to 1 if <0
	(loop for i from 0 to (- (length *color-pool*) 1)
	do
	(setf (nth 1 (nth i *color-pool*)) (if (> (- 100 (nth 1 (nth i *color-pool*))) 0) (- 100 (nth 1 (nth i *color-pool*))) 1)))
)


;; GENERATE-CRAPPY-GUESS: Given a number of red and white pegs, generate a new 
;; guess from the CFG based on reds and whites.
;; 
;; This function may return duplicate guesses. GENERATE-GUESS should use this
;; function and a hash table to generate unique guesses.
;; 
;; This function is based on section 3.1 of the Temporel/Kovacs paper.
(defun generate-crappy-guess (reds whites)
  (setq cfg-copy (copy-list *cfg*))
  (setq white-list '())                     ;; white indices to swap
  (setq red-list '())                       ;; red indices to leave alone
  (setq index-list (range 0 *mm-code-length*)) ;; list of indices into *cfg*


  ;; create list of red indices to leave alone
  (loop for i from 0 below reds do
        (setq index (random (length index-list)))
        ;; index is the index of the element in index-list which should be
        ;; removed from index-list and placed in red-list. swap is the element
        ;; at that location.
        (setq swap (nth index index-list))
        (setq red-list (cons swap red-list))
        (setq index-list (remove swap index-list)))


  ;; create a list of white indices to swap
  (loop for i from 0 below whites do
        (setq index (random (length index-list)))
        ;; index is the index of the element in index-list which should be
        ;; removed from index-list and placed in white-list. swap is the element
        ;; at that location.
        (setq swap (nth index index-list))
        (setq white-list (cons swap white-list))
        (setq index-list (remove swap index-list)))


   (change-color-pool-prob red-list white-list)


  ;; swap white colors into new position
  (setq swap-locations (append white-list index-list))
  (loop for white-index in white-list do
	(setq this-color (nth white-index *cfg*))
       
	(setq index (random (length swap-locations)))
	(setq new-location (nth index swap-locations))
	(setq swap-locations (remove new-location swap-locations))

	(setf (nth new-location cfg-copy) this-color))

  ;; for any remaining indices into swap-locations, choose a new color and 
  ;; set cfg-copy[current-index] = to that color
  (loop for index in swap-locations do
	(setf color-index (prob-random-color (sum-prob)))
	(setf color (car (nth color-index *color-pool*)))
	(setf (nth index cfg-copy) color))
  
;   (reset-color-pool-prob)

  cfg-copy)


  
	
;;Runs generate-guess untill it gets a valid guess or if the super-constant number of guesses is reached	
(defun generate-valid-guess (reds whites)
  (setq guess (generate-guess reds whites))
  (let ((trys 0) (out-distance (car (is-valid-guess guess)))(distance (+ (nth 0 (is-valid-guess guess)) (* .10 (nth 1 (is-valid-guess guess)))) ) (best-guess guess) (best-distance 0))
	(setf best-distance distance)
  	(loop while (not (eq out-distance 0)) do
		(setq guess (generate-guess reds whites))
		(setf out-distance (nth 0 (is-valid-guess guess)))
		(setf distance (+ out-distance (* .10 (nth 1 (is-valid-guess guess)))) )
		(if (< distance best-distance)
			(progn 
				(setf best-distance distance)
				(setf best-guess guess)
			)
		)
		(setf trys (+ trys 1))
		(if (> trys *superconstant*)
		   (progn
	 	 	(return-from generate-valid-guess best-guess)
		   )
		)
	  )
	(setf *num-guesses-evaluated* (+ *num-guesses-evaluated* trys))
  ) 


  guess)
	

