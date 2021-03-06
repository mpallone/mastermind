; some utility functions 

(if (not (find-package :mm-markpa1)) (make-package :mm-markpa1))
(in-package :mm-markpa1)
;; mm.lisp should already have been loaded!
(use-package :mm)



;; return the sum of the elements in list l
(defun sum (l)
  (setq s 0)
  (loop for i in l
        do

        (setq s (+ s i)))
  s)

;; return the average of the elements in list l
(defun avg(l)
  (/ (sum l) (length l)))

;; return the standard deviation of the elements
;; in list l
(defun std-dev (l)
  (setq mean (avg l))
  (setq n (length l))
  (setq s 0)

  (setq s (sum (loop for i in l
                     collect (expt (- mean i) 2))))

  (setq s (/ s (- n 1)))
  (setq s (sqrt s))
  s)

; do x number of tests and print some statistics
(defun mm-stat (colors code-length x)
  (setf total-evals 0)
  (setf total-guesses-scored 0)
  (setf total-guesses-generated 0)
  (setf start (get-internal-run-time))

  (setf list-of-valid-counts '())
  (setf list-of-guesses-scored-counts '())
  (setf list-of-guesses-generated-counts '())

  (loop for i from 1 to x do
	(format t "test ~s ~&" i)
        (mm-tournament :colors colors
                       :code-length code-length
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
  (setf finish (get-internal-run-time))

  (format t "avg guesses scored:        ~s ~&" (float (avg list-of-guesses-scored-counts)))
  (format t "avg guesses evaluated:     ~s ~&" (float (avg list-of-valid-counts)))
  (format t "avg guesses generated:     ~s ~&" (float (avg list-of-guesses-generated-counts)))
  (format t "avg seconds per solution:  ~s ~&" (float ( / (/ (- finish start) 1000000) x)))

  )

;; do a variable number of tests based on colors
;; and the specified code-length
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

;; find the best upper limit of commutativity checks for 
;; this problem size and print it out
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

;; run x number of tests and 
;; print out some statistics
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

