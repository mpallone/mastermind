;; Tournament submission - 2:18 PM

;; Max Spector and Mark Pallone
;; mspecto1@umbc.edu         markpa1@umbc.edu
;; 
;; CMSC 471 Dry Run
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
(defun number-to-guess (number)
	(let ((temp-cfg '()))
		(loop for i from 0 to (- (length number) 1) do
			(setf temp-cfg (append temp-cfg (list (nth (nth i number) *mm-colors*)) ))		
		)
		temp-cfg
	)
)


(defun flex-test(colors code-length rounds)
  (let (start '() finish '())

    (setq start (get-internal-run-time))
    (mm-tournament :colors colors
                   :code-length code-length
                   :gen-fn 'mm-gen-random
                   :players (list *mm-gentest-player*)
                   :rounds rounds)
    (setq finish (get-internal-run-time))
    (format t "flex-test ran in ~s microseconds.~&" (- finish start))
    (format t "(~s seconds)" (float (/ (- finish start) 1000000)))))




;;=======================================================================
;; GLOBAL VARIABLE

;; Global variable that stores a player instance for
;; the generate-and-test player
(defvar *num-considered-guesses*) ;; how many guesses were considered in total, including ones that weren't "offic\ially" guessed?
(defvar *num-guesses-evaluated*) ;; how many guesses are sent to isValidGuess?
(defvar *high-punish-score*) ;; that max score a guess can still receive, and still have its probabilities reduced.
(defvar *punish-percentage*) ;; the bottom % of scores that are to be punished (ranging from 0 to maximum possible score)

(defvar *mm-gentest-player*)
(defvar *color-pool*)
(defvar *cfg*)
(defvar *pre-guesses*)
(defvar *prev-considered*)
(defvar *mm-colors*)
(defvar *mm-code-length*)
(setf *superconstant* 0)

(setf *punish-percentage* .14)



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
(defun add-cfg-to-prev ()
	(setf *prev-guesses* 
		(let () 
			(multiple-value-setq (red white) (mm-score (number-to-guess *cfg*) *code* *mm-colors*))
			(list (list *cfg* (list red white)))
		)
	)
)
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


(defun constant-test (x step min-constant max-constant)
	(setf *superconstant* min-constant)
	(let ((min-guesses (list 999 0)) (min-time (list 999 0)) (x-test-return nil))
		(loop while (< *superconstant* max-constant) do 
			(format t "Constant: ~d~%" *superconstant*)
			(setf x-test-return (x-test x))
			(if (< (nth 0 x-test-return) (nth 0 min-guesses))
				(progn
					(setf (nth 0 min-guesses) (nth 0 x-test-return))
					(setf (nth 1 min-guesses) *superconstant*)
					(format t "new best constant ~d: ~d guesses~%" *superconstant* (nth 0 x-test-return))
				)
			)
			(if (< (nth 1 x-test-return) (nth 0 min-time))
				(progn
					(setf (nth 0 min-time) (nth 1 x-test-return))
					(setf (nth 1 min-time) *superconstant*)
					(format t "new best constant ~d: ~d seconds~%" *superconstant* (nth 1 x-test-return))
				)
			)
			(setf *superconstant* (+ *superconstant* step))
		)
		(format t "best constant ~d: ~d guesses~%" (nth 1 min-guesses) (nth 0 min-guesses))
		(format t "best constant ~d: ~d seconds~%" (nth 1 min-time) (nth 0 min-time))
	)

)

;; do x number of tests for each value to be tested
(defun punish-tests (x)
  (let ((test-values '(.05 .06 .07 .08 .09 .10 .11 .12 .13 .14 .15 .16 .17 .18 .19 .20)))
    (loop for percentage in test-values do
	  (format t "testing value ~s ~&" percentage)
	  (setq *punish-percentage* percentage)
	  (mm-stat '(a b c d e f g h) 8 x)
	  (print '------------))))
	  

(defun x-test (x)
	(setf total 0)
	(setf total-time 0)
	(setf current-time (get-universal-time))
	(setf max-time 0)
	(setf max-guess 0)
	(loop for i from 1 to x do
		;(format t "test ~s" i)
		(setf *guesses* 0)
		(mm-gen-random)
		(mm-gentest)
		(setf total-time (+ total-time (- (get-universal-time) current-time)))
		;(format t "took ~d seconds~%" (- (get-universal-time) current-time))
		(if (> (- (get-universal-time) current-time) max-time) (setf max-time (- (get-universal-time) current-time)))
		(if (> *guesses* max-guess) (setf max-guess *guesses*))
		(setf current-time (get-universal-time))
		(setf total (+ total *guesses*))
		
	)
	(format t "~d tests~%" x)
	(format t "average time ~d seconds~%" (float (/ total-time x)))
	(format t "average guesses ~d~%" (float (/ total x)))
	(format t "max guesses ~d~%" max-guess)
	(format t "max time ~d seconds~%" max-time)
	(list (float (/ total x)) (float (/ total-time x)))
)

(defun mm-gentest (&optional (colors *colors*) (code-length *code-length*)
			     (generator #'mm-gen-random)
			     &aux reds whites)
	(setf *mm-colors* colors)

	(setf *mm-code-length* code-length)
  "Super 1337 guesser"
	(reset-color-pool)
	(setf *prev-considered* (make-hash-table :test #'equal))
	(setf *cfg* (generate-cfg))

	;; statistical stuff
	(setf *num-considered-guesses* 0)
	(setf *num-guesses-evaluated* 0)

	
	;; set the punishable score to be *punish-percentage* of the highest possible score
	(setq maximum-score (basic-heuristic code-length 0))
	(setq *high-punish-score* (* *punish-percentage* maximum-score))

	(add-cfg-to-prev)	
;(format t "Guessing ~s...~%" (number-to-guess *cfg*))
	(setf reds (car (car (cdr (car (last *prev-guesses*))))))
	(setf whites (car (cdr (car (cdr (car (last *prev-guesses*)))))))
 
	(let ((guess *cfg*) (red 0) (white 0) (time (get-universal-time)))
		(loop while (not (eq red *mm-code-length*)) do
		        ;(print red)
			;(print white)
		        ;(print 'generating_guess)
			(setf guess (generate-valid-guess reds whites))
			;(print 'printing_guess)
			;(print 'getting_score)
			(multiple-value-setq (red white) (mm-score (number-to-guess guess) *code* *mm-colors*))
;			(format t "Guessing ~s got reds: ~d whites: ~d =~d~%" (number-to-guess guess) red white (basic-heuristic red white))
			;(print 'set_prev-guesses)
			(setf *prev-guesses* (cons (list guess (list red white)) *prev-guesses*))
			;(print 'checking_heuristic)
			(setq this-score (basic-heuristic red white))
			(if (> this-score (basic-heuristic reds whites))
				(progn (setf *cfg* guess)
				       (setf reds red)
				       (setf whites white)
					;(format t "New cfg: ~s~%" (number-to-guess *cfg*))
				)
			)

			;; if guess scores <= high-punish-score, reduce probabilities based on the score
			(if (<= this-score *high-punish-score*)

			    (progn
;			        (print " ******** REDUCING SCORE ************ ")
			      (if (eq 0 this-score)
				  (setq punish-percent 1)
				(setq punish-percent (/ (float this-score) *high-punish-score*)))
			      (reduce-list guess punish-percent)))
			

;			(format t "~d seconds~%" (- (get-universal-time) time))
			(setf time (get-universal-time))
			;(read)
	       )

	     

		;(print 'we_win)
;		(format t "mm-gentest guessed ~s correctly in ~s guesses!~%"
;			     (number-to-guess guess) *guesses*)
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
  ;(format t "is-valid-guess() is running with guess = ")
  ;(print guess)
  ;(print "----------")
	(let ((distance 0)) ;(return t)
		(loop for i from (- (length *prev-guesses*) 1) downto 0 do
			(multiple-value-setq (red white) (mm-score (number-to-guess guess) (number-to-guess (car (nth i *prev-guesses*))) *mm-colors* t))
;		(print "checking against")
 ;             (print (car (nth i *prev-guesses*)))
		;(print red)
		;(print (car (car (cdr (nth i *prev-guesses*)))))
		;(print (not (eq red (car (car (cdr (nth i *prev-guesses*)))))))

		;(print white)
		;(print (car (cdr (car (cdr (nth i *prev-guesses*))))))
		;(print (not (eq white (car (cdr (car (cdr (nth i *prev-guesses*))))))))
;weighted		(setf distance (* (+ distance (abs (- (basic-heuristic red white) (basic-heuristic (car (car (cdr (nth i *prev-guesses*)))) (car (cdr (car (cdr (nth i *prev-guesses*))))) )))) (basic-heuristic (car (car (cdr (nth i *prev-guesses*)))) (car (cdr (car (cdr (nth i *prev-guesses*))))) ) ))
			(setf distance (+ distance (abs (- (basic-heuristic red white) (basic-heuristic (car (car (cdr (nth i *prev-guesses*)))) (car (cdr (car (cdr (nth i *prev-guesses*))))) )))) )

			;(if (or (not (eq red (car (car (cdr (nth i *prev-guesses*)))))) (not (eq white (car (cdr (car (cdr (nth i *prev-guesses*))))))) ) 
			;	(setf return nil)
			;)
		)
	(return-from is-valid-guess distance)
	)
	;(print 'valid)
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


;; GENERATE-GUESS
(defun generate-guess (reds whites)
  ;(print "running generate-guess()")
  (setq guess (generate-crappy-guess reds whites))
  (setf guess-string (list-to-string guess))
  
  ;; if the guess has been seen before, generate a new one until you get a 
  ;; unique one
  (let ((trys 0))
 	(loop while (gethash guess-string *prev-considered*) 
     	
		do

		(setq guess (generate-crappy-guess reds whites))
		(setf guess-string (list-to-string guess))
		(setf trys (+ trys 1))
		;(if (> trys 10)
		;	(progn 
		;	  (setf (gethash guess-string *prev-considered*) guess-string)
		;	    (format t "hash trys: ~d~&" trys)
  		;	   (return-from generate-guess guess)
		;	)
		;)

	;	(read)
	
 	 )
	  (setf (gethash guess-string *prev-considered*) guess-string)
	  (setf *num-considered-guesses* (+ *num-considered-guesses* trys))
  )
  ;(if (> trys 0)
  ;(format t "hash trys: ~d~&" trys))

  guess)



(defun reset-color-pool-prob ()
	(loop for i from 0 to (- (length *color-pool*) 1)
	do
	(setf (nth 1 (nth i *color-pool*)) 100))
)

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


  
	
	
(defun generate-valid-guess (reds whites)
  ;(print "calling generate-valid-guess()")
;  (format t "reds: ~d~&" reds)
;  (format t "whites ~d~&" whites)
  (setq guess (generate-guess reds whites))
  ;(print 'gen_valid)
  (let ((trys 0) (distance (is-valid-guess guess)) (best-guess guess) (best-distance 0))
	(setf best-distance distance)
  	(loop while (not (eq distance 0)) do
		(setq guess (generate-guess reds whites))
		(setf distance (is-valid-guess guess))
		(if (< distance best-distance)
			(progn 
				(setf best-distance distance)
				(setf best-guess guess)
			)
		)
		(setf trys (+ trys 1))
		;(if (or (and (< *guesses* 25)  (> trys 50)) (> trys 300))
		(if (> trys 2200)
		 ;(if (> trys *superconstant*)
		   (progn
			; (format t "valid trys: ~d~&" trys)
	 	 	(return-from generate-valid-guess best-guess)
		   )
		)
	  )
	(setf *num-guesses-evaluated* (+ *num-guesses-evaluated* trys))
  	;(if (> trys 0)
  	;(format t "valid trys: ~d~&" trys))	
  ) 


  guess)
	

