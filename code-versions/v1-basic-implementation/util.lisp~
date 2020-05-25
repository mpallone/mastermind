; some utility functions 

(if (not (find-package :mm-markpa1)) (make-package :mm-markpa1))
(in-package :mm-markpa1)
;; mm.lisp should already have been loaded!
(use-package :mm)




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
