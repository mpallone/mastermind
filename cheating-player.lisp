;; Mark Pallone
;; Test Player for just getting used to the infastructure code

(if (not (find-package :mm-mdj)) (make-package :mm-mdj))
(in-package :mm-mdj)
(use-package :mm)

(defvar *mark-player*)
(setf *mark-player*
      (make-instance 'mm::player
		     :name 'mark-gentest
		     :guess-fn 'mark-gentest))

(defun mark-test ()
  (mm-tournament :colors '(mm::r mm::b mm::y)
		 :code-length 3
		 :gen-fn 'mm-gen-random
		 :players (list *mark-player*)
		 :rounds 2))

(defun herp-derp ()
  (format t "herp-derp."))

(defun mark-gentest (&optional (colors *colors*) (code-length *code-length)
			     (generator #'mm-gen-random)
			     &aux reds whites)
  "Cheating generator just to get used to the mm code"
  (multiple-value-setq (reds whites) 
		       (mm-score *code* *code* colors))
  (cond ((eq reds code-length)
	 (format t "cheating player guessed correctly -_-"))
	(t (format t "cheating player didn't guess correctly o.O"))))