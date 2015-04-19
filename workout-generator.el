(defun ay/dayone-workout-warmup-sets (exercises)
  "Create a DayOne entry which creates the warm up sets for the day."
  (interactive)
  (let* ((tmp-file (make-temp-file "emacs-dayone"))
         (cmd (format "%s < %s" emacs-dayone-cmd tmp-file))
         (content (concat
                   "# Workout Plan\n\n"
                   "#workout-plan\n\n"
                   (ay/workout-warmup-sets exercises)
                   ;; (ay/workout-other-sets exercises)
                   )))
    (with-temp-file tmp-file (insert content))
    (if (string-match emacs-dayone-succes-reg (shell-command-to-string cmd))
        (message "Success: contents saved")
      (message "Failed: can't saved"))
    (delete-file tmp-file)))

(defun ay/round-up (x factor)
  (+ (- x (mod x factor))
     (if (> (mod x factor) 0)
         factor 0)))


(defun ay/bar-weights (rep-weight)
  (if (<= (- rep-weight 45) 0)
      0
    (/ (- rep-weight 45) 2)
    ))

(defun ay/workout-set-list (x)
  (if x
      (let* ((multiplier (nth 0 x))
             (rep-weight  (* (nth 1 exercise) multiplier))
             (sets (nth 2 x))
             (reps (nth 1 x)))
        (if (and (< rep-weight 45)
                 (not (= multiplier 0)))
            ""
          (mapconcat (lambda (y) y)
                     (cl-loop for i from 1 to sets collect
                              (let ((rounded-rep-weight (ay/round-up rep-weight 2.5)))
                                (format "- [ ] %3.1f lbs x 1 sets x %d reps (2 x Weights: %3.2f)\n"
                                        (if (= rep-weight 0) 45 rounded-rep-weight)
                                        reps
                                        (ay/bar-weights rounded-rep-weight))))
                     "")
          )) ""))

(defun ay/workout-set-writer (exercise)
  (if (member (nth 0 exercise) '("squat" "bench" "deadlift" "press" "power-clean"))
      (concat
       (format "## %s\n\n" (capitalize (nth 0 exercise)))
       (mapconcat 'ay/workout-set-list
                  (cond ((string= (nth 0 exercise) "squat") squat-jumps)
                        ((string= (nth 0 exercise) "bench") bench-jumps)
                        ((string= (nth 0 exercise) "deadlift") deadlift-jumps)
                        ((string= (nth 0 exercise) "press") press-jumps)
                        ((string= (nth 0 exercise) "power-clean") power-clean-jumps)
                        (t '()))
                  "")
       "\n")))

(defun ay/workout-warmups ()
  (concat "## Warmups\n\n"
          ""))

(defun ay/workout-cooldown ()
  (concat "## Cooldown\n\n"
          " - [ ] Foam Roll\n\n"
          "## Weight\n\nPounds: \n\n"
          "## Meals Eaten\n\n"
          "## Notes\n\n"))

(defun ay/workout-warmup-sets (exercises)
  "Generate the warm up sets for the Starting Strength routine"
  (let ((bar-weight 45)
        (squat-jumps
         (list '(0 5 2)
               '(0.4 5 1)
               '(0.6 3 1)
               '(0.8 2 1)
               '(1.0 5 3)))
        (bench-jumps
         (list '(0 5 2)
               '(0.5 5 1)
               '(0.7 3 1)
               '(0.9 2 1)
               '(1.0 5 3)))
        (deadlift-jumps
         (list '(0 5 2)
               '(0.4 5 1)
               '(0.6 3 1)
               '(0.85 2 1)
               '(1.0 5 1)))
        (press-jumps
         (list '(0 5 2)
               '(0.55 5 1)
               '(0.7 3 1)
               '(0.85 2 1)
               '(1.0 5 3)))
        (power-clean-jumps
         (list '(0 5 2)
               '(0 5 1)
               '(0.55 5 1)
               '(0.7 3 1)
               '(0.85 2 1)
               '(1.0 3 5)))
        (weight-increments '(45 25 10 5 2.5)))
    (concat  "# Workout Plan\n\n"
             (ay/workout-warmups)
             (mapconcat 'ay/workout-set-writer exercises "")
             (ay/workout-cooldown))))
