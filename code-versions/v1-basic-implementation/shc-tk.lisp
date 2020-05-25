;; Max Spector and Mark Pallone
;; mspecto1@umbc.edu         markpa1@umbc.edu
;; 
;; CMSC 471 Dry Run
;; 11/17/2011
;; 
;; Implementation of the Temporel & Kovacz mastermind solver, before "tweaks." 


;; TODO
;; ----
;; 
;; -Print out examinations to make sure our output matches the paper's output. 
;;
;; -Once we're sure the algorithm is implemented correctly, print out how well
;;  it scales for different problem sizes and put it in a spreadsheet. 
;; 
;; -Come up with ideas for tweaking the algorithm (the paper suggests several)
;;  and then try them and record the results in the spreadsheet. 
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
  (mm-tournament :colors '(mm::r mm::b mm::y) 
		 :code-length 4
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
(defvar *num-considered-guesses*) ;; how many guesses were considered in total, including ones that weren't "officially" guessed? 
(defvar *num-guesses-evaluated*) ;; how many guesses are sent to isValidGuess? 
(defvar *mm-gentest-player*)
(defvar *color-pool*)
(defvar *cfg*)
(defvar *pre-guesses*)
(defvar *prev-considered*)
(setf *prev-considered* (make-hash-table :test #'equal))
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
(defun reset-color-pool ()
	(setf *color-pool*
		(let ((temp-color-pool '()))
			(loop for i from 0 to (- (length *colors*) 1) do
				(setf temp-color-pool (append temp-color-pool (list (list i 100)) ))
			)
			temp-color-pool
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
(defun add-cfg-to-prev ()
	(setf *prev-guesses* 
		(let () 
			(multiple-value-setq (red white) (mm-score (number-to-guess *cfg*)))
			(list (list *cfg* (list red white)))
		)
	)
)
(setf *mm-gentest-player*
      (make-instance 'mm::player
		     :name 'mm-gentest
		     :guess-fn 'mm-gentest))



;;Test stuff:


(defun test-my()
;	(print *color-pool*)
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
(defun x-test (x)
	(setf total 0)
	(loop for i from 1 to x do
;		(format t "test ~s" i)
		(mm-gentest)
		(setf total (+ total *guesses*))
	)
	(float (/ total x))
)
(defun mm-gentest (&optional (colors *colors*) (code-length *code-length*)
			     (generator #'mm-gen-random)
			     &aux reds whites)
  "Super 1337 guesser"
  
       (setf *guesses* 0)
       (setf *num-considered-guesses* 0)
       (setf *num-guesses-evaluated* 0)
       (setf start-time (get-universal-time))
	(mm-gen-random)
	(reset-color-pool)
	(setf *prev-considered* (make-hash-table :test #'equal))
	(setf *cfg* (generate-cfg))
	(add-cfg-to-prev)	
;(format t "Guessing ~s...~%" (number-to-guess *cfg*))
	(setf reds (car (car (cdr (car (last *prev-guesses*))))))
	(setf whites (car (cdr (car (cdr (car (last *prev-guesses*)))))))
 
	(let ((guess *cfg*) (red 0) (white 0))
		(loop while (not (eq red *code-length*)) do
		        ;(print red)
			;(print white)
		        ;(print 'generating_guess)
			(setf guess (generate-valid-guess reds whites))
			;(print 'printing_guess)
			;(print 'getting_score)
			(multiple-value-setq (red white) (mm-score (number-to-guess guess)))
			(format t "Guessing ~s got reds: ~d whites: ~d =~d~%" (number-to-guess guess) red white (basic-heuristic red white))
			;(print 'set_prev-guesses)
			(setf *prev-guesses* (cons (list guess (list red white)) *prev-guesses*))
			;(print 'checking_heuristic)
			(if (> (basic-heuristic red white) (basic-heuristic reds whites))
				(progn (setf *cfg* guess)
				       (setf reds red)
				       (setf whites white)
					(format t "New cfg: ~s~%" (number-to-guess *cfg*)))
			)
			;(read)
	       )
		;(print 'we_win)
		(format t "mm-gentest guessed ~s correctly in ~s guesses!~%"
			      (number-to-guess guess) *guesses*)
		(setf stop-time (get-universal-time)) 
		(setf runtime (- stop-time start-time))
		(format t "total runtime: ~s seconds.~%" runtime)
	)

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
  
;  (format t "is-valid-guess() is running with guess = ")
;  (print guess)
  ;(print "----------")
	(loop for i from (- (length *prev-guesses*) 1) downto 0 do
		(multiple-value-setq (red white) (mm-score (number-to-guess guess) (number-to-guess (car (nth i *prev-guesses*))) *colors* t))
;		(print "checking against")
 ;             (print (car (nth i *prev-guesses*)))
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
	;(print 'valid)
	t
)



;; BASIC-HEURISTIC: given a number of reds and whites, and the total number of
;; pegs in the code, return a score corresponding the "value" of that number
;; of reds and whites. This heuristic comes from section 3.2 of the 
;; Temporel/Kovacs paper. 
(defun basic-heuristic (reds whites)
  ;; These two error conditions shouldn't happen if Dr. DJ wrote her scoring
  ;; function correctly.

  ;; can't ever have more reds and whites than pegs. Just return 0. 
  (if (< *code-length* (+ reds whites))
      (return-from basic-heuristic 0))
  ;; this is an impossible situation; in this case just return a large number.
  (if (and (eq whites 1) (eq (- *code-length* 1) reds))
      (return-from basic-heuristic (expt reds reds)))

  (setq 
   score
   (+ (/ (+ (expt whites 2) whites) 2)
      (* reds whites) 
      (/ (+ (expt reds 2) reds) 2)
      reds))
  
;  (format t "basic-heuristic returning: ~s~&" score)
  score)


;; RANGE: Given a start and end value, return a list going from the start number
;; going to the ending number. Analogous to range() in python. 
(defun range (start end)
  (loop for i from start below end collect i))


;; GENERATE-GUESS
(defun generate-guess (reds whites)
;  (print "running generate-guess()")
;  (format t "reds:   ~s~&" reds)
;  (format t "whites: ~s~&" whites)

  (setq guess (generate-crappy-guess reds whites))
  (setf guess-string (list-to-string guess))
  
  ;; if the guess has been seen before, generate a new one until you get a 
  ;; unique one
    (setf trys 0)
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
  ;(if (> trys 0)
  ;(format t "hash trys: ~d~&" trys))
  (setf (gethash guess-string *prev-considered*) guess-string)
  (setf *num-considered-guesses* (+ *num-considered-guesses* trys))
  guess)



(defun reset-color-pool-prob ()
	(loop for i from 0 to (- (length *color-pool*) 1)
	do
	(setf (nth 1 (nth i *color-pool*)) 100))
)

(defun change-color-pool-prob (red-list white-list)
	(loop for i from 0 to (- (length *color-pool*) 1)
	do
	(setf (nth 1 (nth i *color-pool*)) 0))

	(loop for i from 0 to (- (length *cfg*) 1)
	do
	(setf (nth 1 (nth (nth i *cfg*) *color-pool*)) (+ (nth 1 (nth (nth i *cfg*) *color-pool*)) 145)))
	
	(loop for i from 0 to (- (length red-list) 1)
	do
	(setf (nth 1 (nth (nth (nth i red-list) *cfg*) *color-pool*)) (- (nth 1 (nth (nth (nth i red-list) *cfg*) *color-pool*)) 100)))

	(loop for i from 0 to (- (length white-list) 1)
	do
	(setf (nth 1 (nth (nth (nth i white-list) *cfg*) *color-pool*)) (- (nth 1 (nth (nth (nth i white-list) *cfg*) *color-pool*)) 100)))
	
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
;  (print "calling generate-crappy-guess")
;  (format t "reds:   ~s~&" reds)
;  (format t "whites: ~s~&" whites)

  (setq cfg-copy (copy-list *cfg*))
  (setq white-list '())                     ;; white indices to swap
  (setq red-list '())                       ;; red indices to leave alone
  (setq index-list (range 0 *code-length*)) ;; list of indices into *cfg*


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
  
  (reset-color-pool-prob)

  cfg-copy)
	
	
	
(defun generate-valid-guess (reds whites)
;  (print "calling generate-valid-guess()")
;  (format t "reds: ~d~&" reds)
;  (format t "whites ~d~&" whites)
  (setq guess (generate-guess reds whites))
  ;(print 'gen_valid)
  (setf trys 0)
  (loop while (not (is-valid-guess guess)) do
	;(print "not valid try again")
	(setq guess (generate-guess reds whites))
	(setf trys (+ trys 1))
;	(if (> trys 10)
;	   (progn
;		 (format t "valid trys: ~d~&" trys)
;	 	 (return-from generate-valid-guess guess)
;	   )
;	)
  )
;  (if (> trys 0)
;  (format t "valid trys: ~d~&" trys))	
  
  (setf *num-guesses-evaluated* (+ *num-guesses-evaluated* trys))
  guess)
	

;; run x number of tests, and return a list containing the number
;; of times isValidGuess() is called.
(defun is-valid-guess-count (x)
  (setf total 0)
  (setf list-of-valid-counts '())
  (loop for i from 1 to x do
	(format t "test ~s" i)
	(mm-tournament :colors '(mm::r mm::b) 
		       :code-length 2 
		       :gen-fn 'mm-gen-random 
		       :players (list *mm-gentest-player*) 
		       :rounds 1)
	(setf list-of-valid-counts (append list-of-valid-counts (list *num-guesses-evaluated*))))
  (format t "average guesses evaluated: ~s ~&" (avg list-of-valid-counts))
  list-of-valid-counts)
  

;; run x number of tests, and return a list containing the number 
;; of times generate-crappy-guess was called.
(defun list-of-guess-counts (x)
  (setf total 0)
  (setf list-of-guess-counts '())
  (loop for i from 1 to x do
	(format t "test ~s" i)
	(mm-tournament :colors '(mm::r mm::b) 
		       :code-length 2 
		       :gen-fn 'mm-gen-random 
		       :players (list *mm-gentest-player*) 
		       :rounds 1)
	(setf list-of-guess-counts (append list-of-guess-counts (list *num-considered-guesses*))))
  (format t "average guesses generated: ~s ~&" (avg list-of-guess-counts))
  list-of-guess-counts)

; do x number of tests and print some statistics
(defun mm-stat (x)
  (setf total-evals 0)
  (setf total-guesses-scored 0)
  (setf total-guesses-generated 0)

  (setf list-of-valid-counts '())
  (setf list-of-guesses-scored-counts '())
  (setf list-of-guesses-generated-counts '())

  (loop for i from 1 to x do
	(mm-tournament :colors '(mm::r mm::b) 
		       :code-length 2 
		       :gen-fn 'mm-gen-random 
		       :players (list *mm-gentest-player*) 
		       :rounds 1)
	
	(setf list-of-valid-counts
	      (append list-of-valid-counts (list *num-guesses-evaluated*)))

	(setf list-of-guesses-generated-counts 
	      (append
	       list-of-guesses-generated-counts
	       (list *num-considered-guesses*)))

	(setf list-of-guesses-scored-counts
	      (append 
	       list-of-guesses-scored-counts
	       (list *guesses*)))
	)
  
  (format t "guesses scored:    ~s ~&" (float (avg list-of-guesses-scored-counts)))
  (format t "guesses evaluated: ~s ~&" (float (avg list-of-valid-counts)))
  (format t "guesses generated: ~s ~&" (float (avg list-of-guesses-generated-counts)))
  )

(defun sum (l)
  (setq s 0)
  (loop for i in l 
	do
	
	(setq s (+ s i)))
  s)

(defun avg(l)
  (/ (sum l) (length l)))

(defun std-dev (l)
  (setq mean (avg l))
  (setq n (length l))
  (setq s 0)

  (setq s (sum (loop for i in l 
		     collect (expt (- mean i) 2))))
	       
  (setq s (/ s (- n 1)))
  (setq s (sqrt s))
  s)
	
  
