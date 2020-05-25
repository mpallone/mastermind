;; mark-code.lisp
;; markpa1@umbc.edu , mark.c.pallone@gmail.com
;; Mark Pallone's code for the CMSC 471 Fall 2011 Final Project
;; ====================================================


(if (not (find-package :mm-mdj)) (make-package :mm-mdj))
(in-package :mm-mdj)
;; mm.lisp should already have been loaded!
(use-package :mm)

;; BASIC-HEURISTIC: given a number of reds and whites, and the total number of
;; pegs in the code, return a score corresponding the "value" of that number
;; of reds and whites. This heuristic comes from section 3.2 of the 
;; Temporel/Kovacs paper. 
(defun basic-heuristic (reds whites pegs)
  ;; These two error conditions shouldn't happen if Dr. DJ wrote her scoring
  ;; function correctly.

  ;; can't ever have more reds and whites than pegs. Just return 0. 
  (if (< pegs (+ reds whites))
      (return-from basic-heuristic 0))
  ;; this is an impossible situation; in this case just return a large number.
  (if (and (eq whites 1) (eq (- pegs 1) reds))
      (return-from basic-heuristic (expt reds reds)))

  (setq 
   score
   (+ (/ (+ (expt whites 2) whites) 2)
      (* reds whites) 
      (/ (+ (expt reds 2) reds) 2)
      reds)))
	
