;;; jackel.el --- org-mode based workout trakcer -*- lexical-binding: t -*-

;; Author: Zachary Romero
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Homepage: https://github.com/zkry/jackel
;; Keywords: tools

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Jackel is a org-mode based workout tracker.

;;; Code:

(require 'org-element)
(require 'seq)

(defgroup jackel nil
  "Emacs Search Tool Aggregator."
  :prefix "jackel"
  :group 'applications)

(defcustom jackel-log-file "~/org-workspace/apps/workout.org"
  "Main workout log file."
  :type 'file
  :group 'jackel)

(defcustom jackel-default-unit :kg
  "Default unit to log in if not specified."
  :type '(choice (const :kg) (const :lb)))

(defcustom jackel-warmup-program
  '(((.6 . 5))
    ((.4 . 5)
     (.8 . 3))
    ((.4 . 5)
     (.6 . 5)
     (.8 . 3))
    ((.4 . 5)
     (.6 . 5)
     (.75 . 3)
     (.9 . 2))
    ((.45 . 5)
     (.6 . 5)
     (.7 . 3)
     (.8 . 2)
     (.9 . 2)))
  "Default unit to log in if not specified.")

(defcustom jackel-set-note-types
  '("drop" "failure")
  "List of predefined notations for sets.")

(defvar jackel-exercises-heading "Exercises"
  "Top-level heading for which the directory of exercises are located.")

(defvar jackel-routines-heading "Routines"
  "Top-level heading for which the directory of exercise routines are located.")

(defvar jackel-workouts-heading "Workouts"
  "Top-level heading for which the directory of workouts are located.")

(defmacro jackel-with-log-file (&rest body)
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (insert-file-contents jackel-log-file)
     ,@body))

(defmacro jackel-with-log-file-write (&rest body)
  (declare (indent 0) (debug t))
  `(with-temp-file jackel-log-file
     (insert-file-contents jackel-log-file)
     ,@body))

(defun jackel--subheadings (elt)
  (let ((elts))
    (org-element-map (org-element-contents elt) 'headline
      (lambda (hl)
        (when (= (org-element-property :level hl) (1+ (org-element-property :level elt)))
          (push hl elts))))
    (nreverse elts)))

(defun jackel--main-heading (heading-title)
  "Return the main heading named HEADING-TITLE."
  (jackel-with-log-file
    (let ((main-hl))
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (hl)
          (when (and (equal (org-element-property :raw-value hl) heading-title)
                     (= (org-element-property :level hl) 1))
            (setq main-hl hl))))
      main-hl)))

(defun jackel--heding-subelements (heading-title) ;; TODO - fix name
  (jackel-with-log-file
    (let ((main-hl (jackel--main-heading heading-title)))
      (unless main-hl
        (user-error "Unable to find %s listing heading in log file" heading-title))
      (jackel--subheadings main-hl))))

(defun jackel-exercise-elements ()
  "Return a list of all the available exercises elements."
  (jackel--heding-subelements jackel-exercises-heading))

(defun jackel-routine-elements ()
  "Return a list of all the available routine elements."
  (jackel--heding-subelements jackel-routines-heading))

(defun jackel-read-routine ()
  "Prompt user to select a routine and return it."
  (let ((selections-ht (make-hash-table :test 'equal)))
    ;; TODO - include summary information about routine
    (dolist (routine-elt (jackel-routine-elements))
      (let ((title (org-element-property :raw-value routine-elt)))
        (puthash title routine-elt selections-ht)))
    (let ((selection (completing-read "Select a routine: " selections-ht nil t)))
      (gethash selection selections-ht))))

(defun jackel--completing-read-elements (elements)
  "Prompt user to select an element from org ELEMENTS."
  (let* ((completion-items (seq-map
                           (lambda (elt)
                             (cons (org-element-property :raw-value elt) elt))
                           elements))
         (selection (completing-read "Exercise: " completion-items nil t)))
    (alist-get selection completion-items nil nil #'equal)))

(cl-defstruct (jackel-exercise-group
               (:copier nil)
               (:constructor jackel-exercise-group-create))
  "Structure representing a group of exercises for the same movement."
  exercise-element
  exercise-units)

(cl-defstruct (jackel-exercise-unit
               (:copier nil)
               (:constructor jackel-exercise-unit-create))
  "Structure representing a unit of exercise, planned or actualized."
  rep-count
  set-type ; :drop :warmup
  reps-in-reserve
  weight)

(defun jackel-parse-exercise-elt (exercise-elt)
  "Parse EXERCISE-ELT, returning list of exercise units."
  (jackel-with-log-file
    (goto-char (point-min))
    (narrow-to-region (org-element-property :contents-begin exercise-elt)
                      (org-element-property :contents-end exercise-elt))

    (let ((units))
      (while (search-forward-regexp "\\([1-9]+\\)x\\([0-9]+\\)" nil t)
        (let ((set-ct (string-to-number (match-string 1)))
              (rep-ct (string-to-number (match-string 2)))
              (line-str (thing-at-point 'line))
              (weight))
          (when (string-match "@\\([1-9][0-9]+\\)\\(kg\\|lbs?\\)?" line-str)
            (let* ((weight-amt (string-to-number (match-string 1 line-str)))
                   (unit (pcase (match-string 2 line-str)
                           ("lb" :lb)
                           ("lbs" :lb)
                           ("kg" :kg))))
              (setq weight (cons weight-amt unit))))
          (dotimes (_ set-ct)
                (push (jackel-exercise-unit-create
                       :rep-count rep-ct
                       :weight weight)
                      units))))
      (nreverse units))))

(defun jackel--read-weight (&optional prompt)
  "Prompt the user to input a weight.
If PROMPT is a string use that as the prompt."
  (let ((weight (read-number (format "%s (%s): " (or prompt "Weight") jackel-default-unit))))
    (cons weight jackel-default-unit)))

(defun jackel--weight-to-string (weight)
  "Return a string representing WEIGHT."
  (let*  ((clean-float-func (lambda (x)
                              (if (and (floatp x) (= (mod x 1) 0))
                                  (floor x)
                                x)))
          (unit-to-string '((:kg . "kg")
                            (:lb . "lb"))))
    (pcase weight
      (`(,amt . ,unit)
       (format "%s%s" (funcall clean-float-func amt) (alist-get unit unit-to-string)))
      ((cl-type null) "")
      (amt
       (format "%s%s" (funcall clean-float-func amt) (alist-get jackel-default-unit unit-to-string))))))

(defun jackel--round-to-nearest (weight nearest)
  "Round WEIGHT to the closest multiple of NEAREST."
  (cond
   ((consp weight)
    (pcase-let* ((`(,amt . ,unit) weight))
      (cons
       (* (round (/ amt nearest) ) nearest)
       unit)))
   ((numberp weight)
    (* (round (/ weight nearest) ) nearest))))

(defun jackel--weight* (a b)
  "Multiply a cons weight by a number."
  (cond
   ((and (consp a) (consp b))
    (error "Can't multiply two weights together"))
   ((consp b)
    (jackel--weight* b a))
   ((and (numberp a) (numberp b))
    (* a b))
   (t
    (pcase-let ((`(,weight . ,unit) a))
      (cons (* weight b) unit)))))

(defun jackel--insert-workout (routine-elt)
  "Insert EXERCISES into workout file."
  (let* ((routine-exercise-elts (jackel--subheadings routine-elt))
         (groups '())
         workout-start)
    (dolist (eelt routine-exercise-elts)
      (let* ((exercise-elt (car (org-element-contents eelt)))
             (exercises (and exercise-elt (jackel-parse-exercise-elt exercise-elt)))
             (group (jackel-exercise-group-create
                     :exercise-element eelt
                     :exercise-units exercises)))
        (push group groups)))
    (setq groups (nreverse groups))
    (let ((routine-name (org-element-property :raw-value routine-elt))
          (exercise-hl (jackel--main-heading jackel-workouts-heading)))
      (if (org-element-contents-begin exercise-hl)
          (goto-char (org-element-contents-begin exercise-hl))
        (goto-char (org-element-end exercise-hl)))
      (setq workout-start (point))
      (insert (format "** %s %s\n\n" routine-name (format-time-string "[%Y-%m-%d %a]")))
      (dolist (group groups)
        (let* ((eelt (jackel-exercise-group-exercise-element group))
               (units (jackel-exercise-group-exercise-units group)))
          (insert (format "*** %s\n" (org-element-property :raw-value eelt)))
          (insert "| Reps | Weight | Note |\n")
          (insert "|------+--------+------|\n")
          (unless units
            (insert "|      |        |      |\n"))
          (dolist (unit units)
            (let* ((rep (jackel-exercise-unit-rep-count unit))
                   (weight (jackel-exercise-unit-weight unit)))
              (insert (format "| %s | %s | %s |\n" rep "" (jackel--weight-to-string weight)))))
          (save-excursion
            (org-table-align))
          (insert "\n"))))
    (goto-char workout-start)
    (org-element-at-point)))

(defvar jackel-workout-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)

    ;; Movement and Viewing
    (keymap-set map "<tab>" #'jackel-workout-next-empty-field)
    (keymap-set map "<backtab>" #'jackel-workout-previous-empty-field)
    (keymap-set map "." #'org-shifttab) ;; something more native?

    (keymap-set map "<up>" #'jackel-workout-up-field)
    (keymap-set map "<down>" #'jackel-workout-down-field)
    (keymap-set map "<left>" #'jackel-workout-left-field)
    (keymap-set map "<right>" #'jackel-workout-right-field)

    (keymap-set map "p" #'jackel-workout-up-field)
    (keymap-set map "n" #'jackel-workout-down-field)
    (keymap-set map "f" #'jackel-workout-left-field)
    (keymap-set map "b" #'jackel-workout-right-field)

    ;; Set Management
    (keymap-set map "+" #'jackel-workout-add-set)
    (keymap-set map "-" #'jackel-workout-remove-set)
    (keymap-set map "'" #'jackel-workout-add-note)
    (keymap-set map "W" #'jackel-workout-add-warmup-set)
    (keymap-set map "n" #'jackel-workout-note-set)

    ;; Exercise Management
    (keymap-set map "E" #'jackel-workout-add-exercise)
    (keymap-set map "k" #'jackel-workout-remove-exercise)
    (keymap-set map "S" #'jackel-workout-swap-exercise)
    ;; (keymap-set map "i" #'jackel-workout-info-dwim)

    (keymap-set map "S-<up>" #'org-move-subtree-up)
    (keymap-set map "S-<down>" #'org-move-subtree-down)

    ;; Data Entry
    (keymap-set map "\"" #'jackel-workout-ditto)
    (dotimes (i 10)
      (keymap-set map (string (+ ?0 i)) #'self-insert-command))

    ;; Session Management
    ; (keymap-set map "r" #'jackel-workout-pause-start-rest)
    ; (keymap-set map "P" #'jackel-workout-pause-start-session)
    ; (keymap-set map "Q" #'jackel-workout-quit)

    map)
  "Mode map for `jackel-workout-mode'")

(defun jackel--table-column-name ()
  "Return the string of the first row of the current column."
  (let ((current-column (org-table-current-column)))
    (when (> current-column 0) ;; we are in a table
      (org-table-get 1 current-column))))

(defun jackel--on-set-line ()
  "Return non-nil if the point is on a line for inputing set data."
  (and (org-at-table-p)
       (> (org-table-current-line) 1)))

(defun jackel--target-weight-of-set (&optional note)
  "Return weight specification.
If NOTE is a string, use that instead of getting the current line's cell."
  (when (and (not note) (not (jackel--on-set-line)))
    (error "No set on current line"))
  (let* ((current-line (org-table-current-line))
         (note-text (or note (org-table-get current-line 3))))
    (cond
     ((string-match "\\([1-9][0-9]*\\(?:\\.[0-9]*\\)?\\)\\(kg\\|lb\\)" note-text)
      (let* ((weight (match-string 1 note-text))
             (unit (match-string 2 note-text)))
        (cons
         (string-to-number weight)
         (intern (concat ":" unit)))))

     ;; unitless string at beginning of note
     ((string-match "\\`\\([1-9][0-9]*\\(?:\\.[0-9]*\\)?\\)[^0-9]" note-text)
      (let* ((weight (match-string 1 note-text)))
        (cons (string-to-number weight) jackel-default-unit))))))

(defun jackel--insert-blank-exercise-table ()
  "Insert a blank exercise table."
  (insert "| Reps | Weight | Note |\n")
  (insert "|------+--------+------|\n")
  (insert "|      |        |      |\n"))

(defun jackel-workout-next-empty-field ()
  "Move the point to the next empty field in a workout session."
  (interactive)
  (let ((starting-point (point))
        (next-field-column-names '("Reps" "Weight"))) ;; TODO: Make this a defcustom
    (catch 'done
     (while t
       (let ((next-field-exists (search-forward-regexp "|\\( \\) *|" nil t)))
         (unless next-field-exists
           (throw 'done nil))
         (if next-field-exists
             (progn
               (goto-char (match-end 1))
               (when (member (jackel--table-column-name) next-field-column-names)
                 (throw 'done (point))))
           (beep)
           (goto-char starting-point)
           (throw 'done (point))))))))

(defun jackel-workout-previous-empty-field ()
  "Move the point to the previous empty field in a workout session."
  (interactive)
  (let ((starting-point (point))
        (next-field-column-names '("Reps" "Weight"))) ;; TODO: Make this a defcustom
    (catch 'done
     (while t
       (let ((next-field-exists (search-backward-regexp "|\\( \\) *|")))
         (if next-field-exists
             (progn
               (goto-char (match-end 1))
               (when (member (jackel--table-column-name) next-field-column-names)
                 (throw 'done nil)))
           (beep)
           (goto-char starting-point)
           (throw 'done nil)))))))

(defun jackel-workout-up-field ()
  "Move the next field above the current one."
  (interactive)
  (let ((current-column (org-table-current-column)))
    (if (= current-column 0)
        (beep)
      (let ((current-line (org-table-current-line)))
        (if (> current-line 2)
            (progn
              (org-table-goto-line (1- current-line))
              (org-table-goto-column current-column))
          ;; Go to previous table's last entry
          (let ((start (point)))
            (org-fold-show-all)
            (org-table-goto-line 1)
            (forward-line -1)
            (search-backward-regexp (concat "| " " *" "Reps"  " *" " |"))
            (org-table-goto-line 9999)
            (org-table-goto-column current-column)))))))

(defun jackel-workout-down-field ()
  "Move the next field below the current one."
  (interactive)
  (let ((current-column (org-table-current-column)))
    (if (= current-column 0)
        (beep)
      (let ((current-line (org-table-current-line)))
        (org-table-goto-line (1+ current-line))
        (org-table-goto-column current-column)
        (when (= (org-table-current-line) current-line)
          (search-forward-regexp (concat "| " " *" "Reps"  " *" " |"))
          (org-table-goto-line 2)
          (org-table-goto-column current-column))))))

(defun jackel-workout-right-field ()
  "Move the next field below the current one."
  (interactive)
  (let ((last-line (save-excursion
                     (goto-char (- (org-table-end) 2))
                     (org-table-current-line)))
        (last-col (save-excursion
                    (goto-char (- (org-table-end) 2))
                    (org-table-current-column)))
        (current-line (org-table-current-line))
        (current-column (org-table-current-column)))
    (if (and (= last-line current-line)
             (= last-col current-column))
        (progn
          (search-forward-regexp (concat "| " " *" "Reps"  " *" " |"))
          (org-table-goto-line 2)
          (org-table-goto-column 1))
      (org-table-next-field))))

(defun jackel-workout-left-field ()
  "Move the next field below the current one."
  (interactive)
  (let ((current-line (org-table-current-line))
        (current-column (org-table-current-column)))
    (if (and (<= current-line 2)
             (= current-column 1))
        (progn
          (goto-char (org-table-begin))
          (search-backward-regexp (concat "| " " *" "Reps"  " *" " |"))
          (let ((last-line (save-excursion
                             (goto-char (- (org-table-end) 2))
                             (org-table-current-line)))
                (last-col (save-excursion
                            (goto-char (- (org-table-end) 2))
                            (org-table-current-column))))
            (org-table-goto-line last-line)
            (org-table-goto-column last-col)))
      (org-table-previous-field))))

(defun jackel-workout-add-set ()
  "Add a set to the current routine."
  (interactive)
  (org-table-insert-row 2))

(defun jackel-workout-remove-set ()
  "Remove the set on the current line."
  (interactive)
  (org-table-kill-row))

(defun jackel-workout-add-note (note)
  "Add NOTE to the current set's note column."
  (interactive "sNote: ")
  (unless (jackel--on-set-line)
    (user-error "No set on current line"))
  (let* ((current-line (org-table-current-line))
         (current-note (org-table-get current-line 3)) ;; TODO - make more general
         (new-note (if (string-blank-p current-note)
                       note
                     (concat current-note "; " note))))
    (org-table-put current-line 3 new-note t)))

(defun jackel-workout-add-warmup-set ()
  "Insert a warmup set at the beginning the exercise."
  (interactive)
  ;; TODO - remove command repea, instaed count how many warmups already exist.
  (let* ((warmup-count 1)
         (set-target-weight))
    (let ((line 2))
      (while-let ((note (org-table-get line 3)))
        (if (string-match-p "warmup" note)
            (cl-incf warmup-count)
          (when (not set-target-weight)
            (setq set-target-weight (jackel--target-weight-of-set note))))
        (cl-incf line)))
    (org-table-goto-line 2)
    (org-table-goto-column 2)
    (let* ((set-target-weight (or set-target-weight
                                  (jackel--read-weight "Warmup leading to weight")))
           (warmup-routine (seq-find
                            (lambda (program)
                              (= (length program) warmup-count))
                            jackel-warmup-program)))
      (org-table-insert-row)
      (dotimes (i warmup-count)
        (pcase-let* ((`(,percent . ,reps) (nth i warmup-routine))
                     (line (+ 2 i))
                     (warmup-set-weight (and percent (jackel--weight* set-target-weight percent))))
          (if reps
              (org-table-put line 1 (number-to-string reps))
            (org-table-put line 1 ""))
          (org-table-put line 3
                         (if warmup-set-weight
                             (concat (jackel--weight-to-string
                                      (jackel--round-to-nearest
                                       warmup-set-weight
                                       0.5)) ;; TODO: Make customizable
                                     "; warmup")
                           "warmup")
                         t))))))

(defun jackel-workout-note-set ()
  "Note the current set as a special type."
  (interactive)
  (let* ((type (completing-read "Note: " jackel-set-note-types)))
    (jackel-workout-add-note type)))

(defun jackel-workout-add-exercise (exercise-elt)
  "Add an exercise to the current routine."
  (interactive
   (list (jackel--completing-read-elements (jackel-exercise-elements))))
  (org-insert-heading-respect-content)
  (let* ((exercise-name (org-element-property :raw-value exercise-elt)))
    (insert exercise-name "\n\n")
    (jackel--insert-blank-exercise-table)
    (org-previous-visible-heading 1)
    (jackel-workout-next-empty-field)))

(defun jackel-workout-remove-exercise ()
  (interactive)
  (org-mark-subtree)
  (delete-region (region-beginning) (region-end))
  (unless (jackel-workout-next-empty-field)
    (jackel-workout-previous-empty-field)))

(defun jackel-workout-swap-exercise (exercise)
  "Replace current exercise with a different EXERCISE."
  (interactive (list (jackel--completing-read-elements (jackel-exercise-elements))))
  (org-mark-subtree)
  (delete-region (region-beginning) (region-end))
  (forward-line -1)
  (jackel-workout-add-exercise exercise))

(defun jackel-workout-ditto ()
  "Copy the value of the cell above for the current cell."
  (interactive)
  (unless (jackel--on-set-line)
    (user-error "No set on current line"))
  (let* ((current-line (org-table-current-line))
         (current-column (org-table-current-column)))
    (if (<= current-line 2)
        (beep)
      (let* ((up-val (org-table-get (1- current-line) current-column)))
        (org-table-put current-line current-column up-val t)))))

(define-minor-mode jackel-workout-mode
  "Toggle `jackel-workout-mode'.
`jackel-workout-mode' provides an easy way to input workout data
by adding certain keybindings and automatically tracking certain changes.")

(defun jackel-start-workout (routine-elt)
  "Create a new workout set for selected routine."
  (interactive
   (list (jackel--completing-read-elements (jackel-routine-elements))))
  (find-file jackel-log-file)
  (widen)
  (let ((workout-heading-elt (jackel--insert-workout routine-elt)))
    (goto-char (org-element-property :contents-begin workout-heading-elt))
    (forward-line -1)
    (org-narrow-to-subtree)))

;;; Exercise Display

(defun jackel-display-exercise (exercise-elt)
  "Display the exercise info buffer for a given org element EXERCISE-ELT."
  (let* ((exercise-name (org-element-property :raw-value exercise-elt))
         (buf (get-buffer-create (format "*jackel - %s*" exercise-name))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert exercise-name "\n")
        (special-mode)))
    (display-buffer buf)))

(defun jackel-browse-exercises (exercise-elt)
  "Display the exercise page for given exercise org element EXERCISE-ELT."
  (interactive
   (list (jackel--completing-read-elements (jackel-exercise-elements))))
  (jackel-display-exercise exercise-elt))

(provide 'jackel)
;;; jackel.el ends here
