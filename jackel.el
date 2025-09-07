;;; jackel.el --- Jackel is an org-mode based workout trakcer -*- lexical-binding: t -*-

;; Author: Zachary Romero
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
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

(require 'org)
(require 'org-element)
(require 'org-clock)
(require 'org-capture)
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
  "Default unit to log in if not specified."
  :type
  '(repeat (repeat (cons float natnum))))


;;; Variables and constants

;; Properties

(defconst jackel-autorest-property-name "JACKEL_AUTOREST"
  "The name of the property which defines how long rest intervals should be.")
(defconst jackel-exercise-type-property-name "JACKEL_EXERCISE_TYPE"
  "Name of the property which defines how an exercise is to be recorded.
It can be any of the following strings: \"weight+reps\", \"distance+time\",
\"reps\", \"time\", or \"weight+time\".

By default, exercises are assumed to be of the type `(weight reps)'.")

(defconst jackel-exercise-muscle-group-property-name "JACKEL_MUSCLE_GROUP"
  "Name of property which stores the musclegroup(s) that the exercise is for.
This should be a comma seperated list of items.")

;; System-heading related

(defvar jackel-exercises-heading "Exercises"
  "Top-level heading for which the directory of exercises are located.")

(defvar jackel-routines-heading "Routines"
  "Top-level heading for which the directory of exercise routines are located.")

(defvar jackel-workouts-heading "Workouts"
  "Top-level heading for which the directory of workouts are located.")

;; Table-related

(defconst jackel-column-name-weight "weight"
  "The column name for recording a weight.")
(defconst jackel-column-name-reps "reps"
  "The column name for recording a rep count.")
(defconst jackel-column-name-distance "distance"
  "The column name for recording a distance.")
(defconst jackel-column-name-time "time"
  "The column name for recording a time.")
(defconst jackel-column-name-notes "notes"
  "The column name for recording a time.")

;; Workout-mode related

(defvar jackel-workout-heading-marker nil
  "Variable to stor the top-level heading position.
For example, used to quickly find the item needed to clock in/out of.")

(defvar-local jackel-fold-setting 'hide-other
  "Stores the auto-folding preference for jackel-workout-mode.
It can be either the symbols `hide-other' so on only the current
set/superset is shown or all, to show all exercises.")
;; TODO: add defcustom  of jackel-default-fold-setting, or document setting default value.

(defvar jackel-insert-reference-point nil
  "Variable storing the spot where input was last run.
This is used to auto-clear fields when inputing a number.")

(defvar jackel-workout-mode-timer nil
  "Timer updating various UI of the current active session.")

(defvar jackel-active-workout-name nil
  "Variable to store the name of the current active routine.")

(defvar jackel-workout-rest-time nil
  "Cons of the float time when the rest timer started and the target rest time.
If nil, the rest timer isn't active.")



;;; Org document structure

(defcustom jackel-exercise-routine-files '()
  "List if files containing exercises and routines.
All files in this list will be scanned in the following manner:

- Any heading with the property \"JACKEL_EXERCISE_TYPE\" or
  \"JACKEL_MUSCLE_GROUP\" will be understood to be an exercise
  definition.

- Any heading with a non-empty \"JACKEL_ROUTINE\" property will
  be understood to be a workout routine.

While you may save workouts in any of the files listed here, they
can be stored separately."
  :type '(repeat file))

;; (setq jackel-exercise-routine-files '("/Users/zacharyromero/org-workspace/apps/workout.org"))

(defcustom jackel-workout-capture-templates'()
  "Capture location of new workouts.
The syntax is simmilar to that of `org-capture-templates', taking
a list of entries.  Each entry is a list with the following items:

description  A short string describing the template, will be shown during
             selection.

target       Specification of where the captured item should be placed
             The following values are valid:

             (file \"path/to/file\")
                Entry will be placed at the top-level of specified file.

             (file+headline \"path/to/file\" \"node headline\")
             (file+headline \"path/to/file\" function-returning-string)
             (file+headline \"path/to/file\" symbol-containing-string)
                Fast configuration if the target heading is unique in the file.

             (file+olp \"path/to/file\" \"Level 1 heading\" \"Level 2\" ...)
             (file+olp \"path/to/file\" function-returning-list-of-strings)
             (file+olp \"path/to/file\" symbol-containing-list-of-strings)
                For non-unique headings, the full outline path is safer

targets      The specification of routines to be selected.  The following
             values are valid:

             nil
                Prompt for all routines.

             (any-tags \"tag1\" \"tag2\" ...)
                Only include routines contining any of the tags specified.

The rest of the entry is a property list of additional options.
Recognized properties are:

 :prepent   Normally new items are appendend at the end of the target location.
            Setting this property to true reverses this."
  :type `(repeat
          (list (string :tag "Description    ")
                (choice :tag "Target location"
                        (list :tag "File"
                              (const :format "" file)
                              file)
                        (list :tag "File & Headline"
				              (const :format "" file+headline)
                              file
				              (choice :tag "Headline"
				                      (string   :tag "Headline")
				                      (function :tag "Function")
				                      (variable :tag "Variable")))
                        (list :tag "File & Outline path"
				              (const :format "" file+olp)
                              file
				              (choice :tag "Outline path"
                                      (repeat :tag "Outline path" :inline t
				                              (string :tag "Headline"))
			                          (function :tag "Function")
			                          (variable :tag "Variable"))))
                (choice (const :format "All" nil)
                        (list (const :format "any-tags" any-tags)
                              (repeat :tag "tags" :inline t
                                      (string :tag "tag")))))))

(defun jackel--elt-exercise-p (elt)
  "Return non-nil if ELT is an exercise element."
  (or (org-entry-get (org-element-property :begin elt) "JACKEL_MUSCLE_GROUP")
      (org-entry-get (org-element-property :begin elt) "JACKEL_EXERCISE_TYPE")))

(defun jackel-extract-exercises ()
  "Return a list of available exercises."
  (let* ((exercises))
    (dolist (file jackel-exercise-routine-files)
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)
        (let* ((parse-tree (org-element-parse-buffer)))
          (org-element-map parse-tree 'headline
            (lambda (hl)
              (when (or (org-entry-get (org-element-property :begin hl) "JACKEL_MUSCLE_GROUP")
                        (org-entry-get (org-element-property :begin hl) "JACKEL_EXERCISE_TYPE"))
                (push (org-element-property :raw-value hl) exercises)))))))
    (nreverse exercises)))

(defun jackel-extract-routines ()
  "Return a list of available routines."
  (let* ((exercises (jackel-extract-exercises)) ;; TODO - to hashmap?? ;; TODO - error if no exercises
         (routines)
         (case-fold-search t))
    (dolist (file jackel-exercise-routine-files)
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)
        (let* ((parse-tree (org-element-parse-buffer)))
          (org-element-map parse-tree 'headline
            (lambda (hl)
              (when (org-entry-get (org-element-property :begin hl) "JACKEL_ROUTINE")
                (let ((parent-name (when-let* ((parent (org-element-lineage hl org-element-all-elements)))
                                     (org-element-property :raw-value parent)))
                      (routine-exercises '()))
                  ;; get sub-exercises
                  (save-restriction
                    (goto-char (org-element-property :begin hl))
                    (org-narrow-to-subtree)
                    (let* ((parse (org-element-parse-buffer 'headline t)))
                      (org-element-map parse 'headline
                        (lambda (hl)
                          (let* ((subelt-title (org-element-property :raw-value hl)))
                            (when (or
                                   (save-excursion
                                     (save-match-data
                                       (when (org-element-property :contents-begin hl)
                                         (goto-char (org-element-property :contents-begin hl))
                                         (search-forward "| note" (org-element-property :contents-end hl) t))))
                                   (seq-find (lambda (ex) (string-match-p (regexp-quote ex) subelt-title)) exercises))
                              (push hl routine-exercises)))))))
                  (push `(,(org-element-property :raw-value hl) . ((exercise-count . ,(length routine-exercises))
                                                                   (parent-name . ,parent-name)))
                        routines))))))))
    (nreverse routines)))

(defun jackel-read-exercise ()
  "Prompt user to select a known exercise."
  (let* ((exercises (jackel-extract-exercises)))
    (completing-read "Exercise: " exercises nil t nil 'jackel-read-exercise)))

(defun jackel-read-routine ()
  "Prompt user to select a known routine."
  (let* ((routines (jackel-extract-routines))
         (max-size (apply #'max (seq-map #'length (seq-map #'car routines))))
         (max-parent-name-size (apply #'max (seq-map (lambda (x)
                                                       (length (alist-get 'parent-name (cdr x))))
                                                     routines)))
         (completion-extra-properties `(:annotation-function
                                        ,(lambda (completion)
                                           (let* ((routine-data (alist-get completion routines nil nil #'equal))
                                                  (exercise-count (alist-get 'exercise-count routine-data))
                                                  (routine-parent-name (alist-get 'parent-name routine-data)))
                                             (format "%s< %s%s %-14s  " (make-string (- (+ max-size 3) (length completion)) ?\s)
                                                     routine-parent-name (make-string (- (+ max-parent-name-size 3)
                                                                                         (length routine-parent-name))
                                                                                      ?\s)
                                                     (pcase exercise-count
                                                       (0 "(empty)")
                                                       (1 "1 exercise")
                                                       (_ (format "%d exercises" exercise-count)))))))))
    (completing-read "Routine: " routines nil t nil 'jackel-read-routine)))

(defun jackel--get-exercise-by-name (name)
  "Return the string of contents of exercise with NAME."
  (catch 'found
    (dolist (file jackel-exercise-routine-files)
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)
        (let* ((parse-tree (org-element-parse-buffer)))
          (org-element-map parse-tree 'headline
            (lambda (hl)
              (when (and (jackel--elt-exercise-p hl)
                         (string-equal (org-element-property :raw-value hl) name))
                (throw 'found
                       (concat
                        (buffer-substring-no-properties (org-element-property :begin hl) (org-element-property :end hl)) "\n"))))))))))

(defun jackel--get-exercise-type (exercise-name)
  "Return the exercise type of exercise name EXERCISE-NAME."
  (let* ((exercise-data (jackel--get-exercise-by-name exercise-name)))
    (with-temp-buffer
      (insert exercise-data)
      (org-mode)
      (jackel--string-to-exercise-type (or (org-entry-get (point-min) "JACKEL_EXERCISE_TYPE") "weight+reps")))))

(defun jackel--get-routine-by-name (name)
  "Return the string of contents of the routine with NAME."
  (catch 'found
    (dolist (file jackel-exercise-routine-files)
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)
        (let* ((parse-tree (org-element-parse-buffer)))
          (org-element-map parse-tree 'headline
            (lambda (hl)
              (when (and (org-entry-get (org-element-property :begin hl) "JACKEL_ROUTINE")
                         (string-equal (org-element-property :raw-value hl) name))
                (throw 'found
                       (concat
                        (buffer-substring-no-properties (org-element-property :begin hl) (org-element-property :end hl)) "\n"))))))))))



;;; Units helper functions

(defun jackel--parse-duration (str)
  "Parse a duration string STR into the number of seconds represented."
  (when (string-match "[0-9]+" str)
    (let* ((number (string-to-number (match-string 0 str)))
           (unit (and (string-match (concat (regexp-opt '("s" "sec" "m" "min" "minute" "second" "hour" "h")) "$") str)
                      (match-string 0 str))))
      (if unit
          (* number
             (pcase unit
               ("s" 1)
               ("sec" 1)
               ("second" 1)
               ("m" 60)
               ("min" 60)
               ("minute" 60)
               ("h" 360)
               ("hour" 360)))
        number))))

(defun jackel--parse-weight (str)
  "Parse a weight string STR into a cons cell of (AMOUNT . UNIT)."
  (when (string-match "[0-9]+" str)
    (let* ((number (string-to-number (match-string 0 str)))
           (unit (and (string-match (regexp-opt '("kg" "lb" "lb")) str)
                      (match-string 0 str))))
      (if unit
          (cons number
                (pcase unit
                  ("kg" :kg)
                  ("lb" :lb)))
        (cons number jackel-default-unit)))))

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

(defun jackel--value-to-string (val)
  "Return a string representing VAL.
Value can be a united number or a plain number."
  (cond
   ((numberp val) (format "%s" val))
   ((consp val)
    ;; TODO - this will need to change when durations and time are added.
    (jackel--weight-to-string val))))

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
  "Multiply a weight A by a scalar amount B.
Arguments may be reversed."
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


;;; Exercises

(defun jackel--string-to-exercise-type (str)
  "Given an string STR return list of input column types."
  ;; "weight+reps\", \"distance+time\", \"reps\", \"time\", or \"weight+time\"
  (pcase str
    ("weight+reps" '(weight reps))
    ("distance+time" '(time distance))
    ("reps" '(reps))
    ("time" '(time))
    ("weight+time" '(weight time))))


;;; Routines

(defun jackel--column-type-to-name (type)
  "Return the column name of a given column TYPE."
  (pcase type
    ('weight jackel-column-name-weight)
    ('reps jackel-column-name-reps)
    ('distance jackel-column-name-distance)
    ('time jackel-column-name-time)
    ('notes jackel-column-name-notes)))

(defun jackel--column-name-to-type (name)
  "Return the column type symbol of a given column NAME."
  (let ((name (downcase name)))
    (cond
     ((equal name jackel-column-name-weight) 'weight)
     ((equal name jackel-column-name-reps) 'reps)
     ((equal name jackel-column-name-distance) 'distance)
     ((equal name jackel-column-name-time) 'time)
     ((equal name jackel-column-name-notes) 'notes))))

(defun jackel--insert-blank-exercise-table (type)
  "Insert a blank exercise table according to TYPE."
  (insert "| ")
  (dolist (type-elt type)
    (pcase type-elt
      ('weight
       (insert jackel-column-name-weight))
      ('reps
       (insert jackel-column-name-reps))
      ('time
       (insert jackel-column-name-time))
      ('distance
       (insert jackel-column-name-distance)))
    (insert " | "))
  (insert jackel-column-name-notes " |\n")
  (if (= (length type) 2)
      (insert "|-+-+-|\n| | | |\n")
    (insert "|-+-|\n| | |\n"))
  (org-table-align)
  (org-table-goto-line 2)
  (org-table-goto-column 1))

;;; Interactive Commands (for routines):

(defun jackel-routine-insert-exercise (exercise-name)
  "Insert a known exercise EXERCISE-NAME after the current point."
  (interactive (list (jackel-read-exercise)))
  (jackel-workout-add-exercise exercise-name))


;;; Workouts

(defun jackel--get-rest-time ()
  "Return the specified rest time at the current point."
  (let* ((rest-str (org-entry-get (point) jackel-autorest-property-name)))
    (when rest-str
      (jackel--parse-duration rest-str))))

(defun jackel--make-note (val type)
  "Return string representation of VAL having meaning TYPE."
  (unless (string-blank-p val)
    (pcase type
      ('reps (format "x%s" val))
      ('weight (jackel--weight-to-string (jackel--parse-weight val))))))

(defun jackel--routine-sets-to-notes (routine-str)
  "Move all inputted data to notes section for ROUTINE-STR.
Routine definitions are meant to store the templates of workouts
so data shoudln't be entered in the non-last columns for
routines.  If this is done, this function is to take that data
and move it to the notes section in a way that is understandable
by the command `jackel-fill-in'."
  (with-temp-buffer
    (insert routine-str)
    (org-mode)
    (goto-char (point-min))
    (while (search-forward (concat "| " jackel-column-name-notes) nil t)
        (org-table-analyze)
        (let* ((line-ct (- (length org-table-dlines) 2))
               (col-ct org-table-current-ncol)
               (col1 (jackel--column-name-to-type (org-table-get 1 1)))
               (col2 (jackel--column-name-to-type (org-table-get 1 2)))
               (_col3 (jackel--column-name-to-type (org-table-get 1 3))))
          (cl-loop for line from 2 to (1+ line-ct)
                   do
                   (if (= col-ct 2)
                       (let* ((note (jackel--make-note (org-table-get line 1) col1))
                              (old-note (org-table-get line 2))
                              (new-note (if (string-blank-p old-note)
                                            note
                                          (concat note "; " old-note))))
                         (org-table-put line 2 new-note)
                         (org-table-put line 1 ""))
                     (let* ((note1 (jackel--make-note (org-table-get line 1) col1))
                            (note2 (jackel--make-note (org-table-get line 2) col2))
                            (note (concat note1 " " note2))
                            (old-note (org-table-get line 3))
                            (new-note (if (string-blank-p old-note)
                                          note
                                        (concat note "; " old-note))))
                       (org-table-put line 1 "")
                       (org-table-put line 2 "")
                       (org-table-put line 3 new-note)))))
        (org-table-align))
    (buffer-string)))

(defun jackel--routine-string-to-workout (routine)
  "Convert a ROUTINE string to a workout entry."
  (let ((case-fold-search t))
    (with-temp-buffer
      (insert routine)
      (org-mode)
      (goto-char (point-min))
      (save-excursion
        (while (search-forward ":jackel_routine:" nil t)
          (delete-region (1- (pos-bol)) (pos-eol))))
      (while (not (looking-at org-heading-regexp))
        (forward-line t))
      (save-excursion
        (let* ((hl-elt (org-element-at-point))
               (hl-name (org-element-property :raw-value hl-elt)))
          (search-forward hl-name)
          (replace-match (concat hl-name " " (format-time-string "[%Y-%m-%d %a]")))
          (org-set-property "JACKEL_WORKOUT_ROUTINE" hl-name)))
      (buffer-string))))

(defun jackel--capture-new-workout (routine-name)
  "Given a routine by ROUTINE-NAME, create new workout entry according to contents."
  ;;; org-capture-goto-target
  (let* ((workout-contents (jackel--routine-sets-to-notes
                            (jackel--routine-string-to-workout
                             (jackel--get-routine-by-name routine-name))))
         (org-capture-entry `("t" "" entry (file "/Users/zacharyromero/org-workspace/apps/workout.org")
                              ,workout-contents
                              :immediate-finish t)))
    (org-capture)
    (org-capture '(16))))

(defun jackel--set-workout-to-active ()
  "Find the workout of the point and make it active, enabling jackel-workout-mode."
  ;; TODO: catch error and print nicer message
  (catch 'done
    (while t
      (let ((at-elt (org-element-at-point)))
        (when (and (eql (org-element-type at-elt) 'headline)
                   (alist-get "JACKEL_WORKOUT_ROUTINE" (org-entry-properties) nil nil 'equal))
          (message "Let's go!")
          (throw 'done nil))
        (org-up-element))))
  (let* ((workout-name (alist-get "JACKEL_WORKOUT_ROUTINE" (org-entry-properties) nil nil 'equal))
         (workout-marker (make-marker)))
    (org-clock-in)
    (set-marker workout-marker (point))
    (setq jackel-workout-heading-marker workout-marker)
    (org-narrow-to-subtree)
    (setq jackel-active-workout-name workout-name)
    (jackel-workout-mode 1)))

(defvar jackel-workout-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)

    ;; Movement and Viewing
    (keymap-set map "<tab>" #'jackel-workout-next-empty-field)
    (keymap-set map "<backtab>" #'jackel-workout-previous-empty-field)

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
    (keymap-set map "e" #'org-table-edit-field)

    ;; Exercise Management
    (keymap-set map "E" #'jackel-workout-add-exercise)
    (keymap-set map "k" #'jackel-workout-remove-exercise)
    (keymap-set map "S" #'jackel-workout-swap-exercise)
    ;; (keymap-set map "i" #'jackel-workout-info-dwim)
    (keymap-set map "v" #'jackel-workout-toggle-view)

    (keymap-set map "S-<up>" #'org-move-subtree-up)
    (keymap-set map "S-<down>" #'org-move-subtree-down)

    ;; Data Entry
    (keymap-set map "<space>" #'jackel-fill-in)
    (keymap-set map "\"" #'jackel-workout-ditto) ;; something more native?

    (dotimes (i 10)
      (keymap-set map (string (+ ?0 i)) #'jackel-self-insert-command))

    ;; Session Management
    (keymap-set map "r" #'jackel-workout-rest)
    (keymap-set map "P" #'jackel-workout-pause-start-session)
    (keymap-set map "Q" #'jackel-workout-quit)

    map)
  "Mode map for `jackel-workout-mode'.")

(defun jackel--table-current-column-type (&optional column)
  "Return the string of the first row of the current column.
If COLUMN is a number, use it instead of the current column."
  (let ((current-column (or column (org-table-current-column))))
    (when (> current-column 0) ;; we are in a table
      (jackel--column-name-to-type (org-table-get 1 current-column)))))

(defun jackel--line-note (&optional input-section)
  "Return the note string for the current line.
If INPUT-SECTION is non-nil, get the first semicolon delimited text."
  ;; TODO: This could be done more stabily by checking for column names.
  (let* ((current-line (org-table-current-line))
         (col3-data (org-table-get current-line 3))
         (res (if (not (string-blank-p col3-data))
                  col3-data
                (org-table-get current-line 2))))
    (if input-section
        (car (string-split res ";"))
      res)))

(defun jackel--on-set-line ()
  "Return non-nil if the point is on a line for inputing set data."
  (and (org-at-table-p)
       (> (org-table-current-line) 1)))

(defun jackel--target-weight-of-set (&optional note)
  "Return weight specification.
If NOTE is a string, use that instead of getting the current line's cell."
  (when (and (not note) (not (jackel--on-set-line)))
    (error "No set on current line"))
  (let* ((note-text (or note (jackel--line-note t))))
    (cond
     ((string-match "\\([1-9][0-9]*\\(?:\\.[0-9]*\\)?\\)\\(kg\\|lb\\)" note-text)
      (let* ((weight (match-string 1 note-text))
             (unit (match-string 2 note-text)))
        (cons
         (string-to-number weight)
         (intern (concat ":" unit)))))

     ;; unitless string at beginning of note
     ((string-match "\\`\\([1-9][0-9]*\\(?:\\.[0-9]*\\)?\\)\\(?:[^0-9]\\|$\\)" note-text)
      (let* ((weight (match-string 1 note-text)))
        (cons (string-to-number weight) jackel-default-unit))))))

(defun jackel--target-reps-of-set (&optional note)
  "Return weight specification.
If NOTE is a string, use that instead of getting the current line's cell."
  (when (and (not note) (not (jackel--on-set-line)))
    (error "No set on current line"))
  (let* ((note-text (or note (jackel--line-note t))))
    (cond
     ((string-match "x\\([1-9][0-9]*\\)" note-text)
      (let* ((reps (match-string 1 note-text)))
        (string-to-number reps))))))

;; TODO: time spec of MM:SS or HH:MM:SS or min s sec h hour second minute
;; TODO: distance spec of mi, km, m, ft, cm, or in.
(defun jackel--extract-table-from-note ()
  "Return a preset value alist of notes column of current line."
  (let* ((maybe-reps (jackel--target-reps-of-set))
         (maybe-weight (jackel--target-weight-of-set))
         (res))
    (when maybe-reps
      (push (cons 'reps maybe-reps) res))
    (when maybe-weight
      (push (cons 'weight maybe-weight) res))
    res))

(defun jackel--apply-fold-setting ()
  "Assuming buffer is unfolded, apply fold settings of `jackel-fold-setting'."
  (pcase jackel-fold-setting
    ('all nil)
    ('hide-other
     (let* ((pos (point)))
       (org-cycle-global 16)
       (org-reveal)
       (goto-char pos)))))

(defmacro jackel--with-workout-fold-settings (&rest body)
  "Run BODY with all headings unfolded.  After BODY apply fold settings."
  (declare (debug t) (indent 0))
  `(progn
     (org-fold-show-all '(headings))
     ,@body
     (jackel--apply-fold-setting)
     (org-table-align)))

(defun jackel--input-column-types ()
  "Return list of downcased column names that contain performance data."
  (list 'time 'distance 'reps 'weight))

(defun jackel--start-rest-timer (&optional target)
  "Set the rest timer.
If TARGET is an integer, set timer for TARGET seconds in the future."
  (setq jackel-workout-rest-time (cons (floor (float-time)) target)))


(defun jackel--add-rest-time (seconds)
  "Add SECONDS to the current rest time.
If no rest time is active, set timer for SECONDS in the future.
If the rest time is over the previous target, set a new target to
SECONDS after now."
  (pcase jackel-workout-rest-time
    (`(,_start . nil)
     (setq jackel-workout-rest-time (cons (floor (float-time)) seconds)))
    (`(,start . ,duration)
     (setq jackel-workout-rest-time (cons start (if duration (+ duration seconds) seconds))))
    ('nil
     (setq jackel-workout-rest-time (cons (floor (float-time)) seconds)))))

(defun jackel--stop-rest-timer ()
  "Clear rest timer, stopping it."
  (setq jackel-workout-rest-time nil))


;; Workout commands:

(defun jackel-workout-next-empty-field ()
  "Move the point to the next empty field in a workout session."
  (interactive)
  (jackel--with-workout-fold-settings
    (let ((rest-time)
          (row-complete))
      (when-let* ((two-column-input (and (org-at-table-p)
                                         (equal (jackel--table-current-column-type 3) 'notes)))
                  (complete (if two-column-input
                                    (and (not (string-blank-p (org-table-get (org-table-current-line) 1)))
                                         (not (string-blank-p (org-table-get (org-table-current-line) 2))))
                                  (not (string-blank-p (org-table-get (org-table-current-line) 1))))))
        (setq row-complete complete)
        (setq rest-time (jackel--get-rest-time)))
      (let ((starting-point (point))
            (next-field-column-names (jackel--input-column-types)))
        (catch 'done
          (while t
            (let ((next-field-exists (search-forward-regexp "|\\( \\) *|" nil t)))
              (unless next-field-exists
                (throw 'done nil))
              (if next-field-exists
                  (progn
                    (goto-char (match-end 1))
                    (when (member (jackel--table-current-column-type) next-field-column-names)
                      (throw 'done (point))))
                (beep)
                (goto-char starting-point)
                (throw 'done (point)))))))
      (when row-complete
        (jackel--start-rest-timer rest-time)))))

(defun jackel-workout-previous-empty-field ()
  "Move the point to the previous empty field in a workout session."
  (interactive)
  (jackel--with-workout-fold-settings
   (let ((starting-point (point))
         (next-field-column-names (jackel--input-column-types)))
     (catch 'done
       (while t
         (let ((next-field-exists (search-backward-regexp "|\\( \\) *|" nil t)))
           (if next-field-exists
               (progn
                 (goto-char (match-end 1))
                 (when (member (jackel--table-current-column-type) next-field-column-names)
                   (throw 'done nil)))
             (beep)
             (goto-char starting-point)
             (throw 'done nil))))))))

(defconst jackel-workout-table-regex
  (concat "| " " *" jackel-column-name-notes " *" " |")
  "Regex which can be searched for to navigate tables.")

(defun jackel--previous-table ()
  "Move the point to the previous workout table."
  (search-backward-regexp jackel-workout-table-regex nil t))

(defun jackel--next-table ()
  "Move the point to the next workout table."
  (search-forward-regexp jackel-workout-table-regex nil t))

(defun jackel-workout-up-field ()
  "Move the next field above the current one."
  (interactive)
  (jackel--with-workout-fold-settings
    (let ((case-fold-search t)
          (current-column (org-table-current-column)))
      (if (= current-column 0)
          (beep)
        (let ((current-line (org-table-current-line)))
          (if (> current-line 2)
              (progn
                (org-table-goto-line (1- current-line))
                (org-table-goto-column current-column))
            ;; Go to previous table's last entry
            (org-fold-show-all)
            (org-table-goto-line 1)
            (forward-line -1)
            (when (jackel--previous-table)
              (org-table-goto-line 9999)
              (org-table-goto-column current-column))))))))

(defun jackel-workout-down-field ()
  "Move the next field below the current one."
  (interactive)
  (jackel--with-workout-fold-settings
    (let ((case-fold-search t)
          (current-column (org-table-current-column)))
      (if (= current-column 0)
          (progn
            (jackel--next-table)
            (org-table-goto-line 2)
            (org-table-goto-column 1))
        (let ((current-line (org-table-current-line)))
          (org-table-goto-line (1+ current-line))
          (org-table-goto-column current-column)
          (when (= (org-table-current-line) current-line)
            (when (jackel--next-table))
            (org-table-goto-line 2)
            (org-table-goto-column current-column)))))))

(defun jackel-workout-right-field ()
  "Move the next field below the current one."
  (interactive)
  (jackel--with-workout-fold-settings
    (let ((case-fold-search t)
          (last-line (save-excursion
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
            (jackel--next-table)
            (org-table-goto-line 2)
            (org-table-goto-column 1))
        (org-table-next-field)))))

(defun jackel-workout-left-field ()
  "Move the next field below the current one."
  (interactive)
  (jackel--with-workout-fold-settings
    (let ((case-fold-search t)
          (current-line (org-table-current-line))
          (current-column (org-table-current-column)))
      (if (and (<= current-line 2)
               (= current-column 1))
          (progn
            (goto-char (org-table-begin))
            (jackel--previous-table)
            (let ((last-line (save-excursion
                               (goto-char (- (org-table-end) 2))
                               (org-table-current-line)))
                  (last-col (save-excursion
                              (goto-char (- (org-table-end) 2))
                              (org-table-current-column))))
              (org-table-goto-line last-line)
              (org-table-goto-column last-col)))
        (org-table-previous-field)))))

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
      (while-let ((note (org-table-get line 3))) ;; TODO: needs to be dynamic
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

(defun jackel-workout-note-set (note)
  "Add a new note NOTE to the current set."
  (interactive "sNote: ")
  (jackel-workout-add-note note))

(defun jackel-workout-add-exercise (exercise-name)
  "Add a exercise named EXERCISE-NAME to the current routine."
  (interactive (list (jackel-read-exercise)))
  (let* ((exercise-type (jackel--get-exercise-type exercise-name)))
    (org-insert-heading-respect-content)
    (insert exercise-name "\n\n")
    (jackel--insert-blank-exercise-table exercise-type)
    (org-previous-visible-heading 1)
    (jackel-workout-next-empty-field)))

(defun jackel-workout-remove-exercise ()
  "Delete the current exercise in workout."
  (interactive)
  (org-mark-subtree)
  (delete-region (region-beginning) (region-end))
  (unless (jackel-workout-next-empty-field)
    (jackel-workout-previous-empty-field)))

(defun jackel-workout-swap-exercise (exercise-name)
  "Replace current exercise with a different exercise named EXERCISE-NAME."
  (interactive (list (jackel-read-exercise)))
  (jackel-workout-remove-exercise)
  (jackel-workout-add-exercise exercise-name))

(defun jackel-workout-toggle-view ()
  "Toggle view mode from viewing current set to viewing all."
  (interactive)
  (if (eql jackel-fold-setting 'all)
      (progn
        (setq jackel-fold-setting 'hide-other)
        (message "View set to hiding other exercises."))
    (setq jackel-fold-setting 'all)
    (message "View set to showing all."))
  (org-fold-show-all '(headings))
  (jackel--apply-fold-setting))

(defun jackel-fill-in ()
  "Fill in the current cell according to note."
  (interactive)
  (let* ((note-data (jackel--extract-table-from-note))
         (cell-value (org-table-get (org-table-current-line) (org-table-current-column)))
         (column-type (jackel--table-current-column-type))
         (two-input-table-p (equal (jackel--table-current-column-type 3) 'notes))
         (col1-val (org-table-get (org-table-current-line) 1))
         (col2-val (org-table-get (org-table-current-line) 2)))
    (cond
     ((and (not (alist-get column-type note-data)) two-input-table-p)
      (let* ((other-col (if (= (org-table-current-column) 1) 2 1))
             (other-col-type (jackel--table-current-column-type other-col))
             (other-col-current-val (org-table-get (org-table-current-line) other-col))
             (other-col-val (jackel--value-to-string (alist-get other-col-type note-data))))
        (if (and other-col-val (string-blank-p other-col-current-val))
            (org-table-put (org-table-current-line) other-col other-col-val)
          (org-table-put (org-table-current-line) other-col ""))))
     ((not (alist-get column-type note-data))
      (beep))
     ((string-blank-p cell-value)
      (let ((note-val (jackel--value-to-string (alist-get column-type note-data))))
        (org-table-put (org-table-current-line) (org-table-current-column) note-val))
      (when (not two-input-table-p)
        (jackel--start-rest-timer)))
     ((not two-input-table-p)
      (org-table-put (org-table-current-line) (org-table-current-column) ""))
     ((string-blank-p col1-val)
      (let ((note-val (jackel--value-to-string
                       (alist-get (jackel--table-current-column-type 1) note-data))))
        (org-table-put (org-table-current-line) 1 note-val))
      (jackel--start-rest-timer))
     ((string-blank-p col2-val)
      (let ((note-val (jackel--value-to-string
                       (alist-get (jackel--table-current-column-type 2) note-data))))
        (org-table-put (org-table-current-line) 2 note-val))
      (jackel--start-rest-timer))
     (t
      (org-table-put (org-table-current-line) 1 "")
      (org-table-put (org-table-current-line) 2 "")))
    (org-table-align)))

(defun jackel--rest-over-p ()
  "Return non-nil if an active rest timer is on and it is over-time."
  (and (and (consp jackel-workout-rest-time) (cdr jackel-workout-rest-time))
       (> (floor (float-time))
          (+ (car jackel-workout-rest-time) (cdr jackel-workout-rest-time)))))

(defun jackel-workout-rest ()
  "Start rest if not started.  Otherwise add 15 seconds to rest."
  (interactive)
  (cond
   ((jackel--rest-over-p)
    (jackel--stop-rest-timer))
   (jackel-workout-rest-time
    (jackel--add-rest-time 15 ))
   (t
    (let* ((rest-str (org-entry-get (point) jackel-autorest-property-name)))
     (jackel--start-rest-timer (when rest-str (jackel--parse-duration rest-str)))))))

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



(defun jackel--insert-reference-point ()
  "Return a point assocciated with current table.
This is used to know when a user asa switched to editing a
different field."
  (save-excursion
    (while (and (not (bolp))
                (not (looking-at-p "|")))
      (forward-char -1))
    (point)))

(defun jackel-self-insert-command ()
  "Insert key, erasing table cell when editing for first time."
  (interactive)
  (let ((ref-point (jackel--insert-reference-point)))
    (when (not (equal jackel-insert-reference-point ref-point))
      (org-table-blank-field)
      (setq jackel-insert-reference-point ref-point))
    (call-interactively #'org-self-insert-command)))

(defun jackel--header-line ()
  "Return the header-line string for current session.
It is expected that this function runs once a second."
  (let* ((clock-time (org-clock-get-clocked-time))
         (clock-hours (/ clock-time 60))
         (clock-minutes (mod clock-time 60))
         (seconds (mod (org-time-convert-to-integer
		                (time-since org-clock-start-time))
                       60))
         (workout-time)
         (rest-time ""))
    (if (and org-clock-current-task
             (string-match-p (regexp-quote jackel-active-workout-name)
                             org-clock-heading))
        (setq workout-time (if (> clock-hours 0)
                               (format "%02d:%02d:%02d" clock-hours clock-minutes seconds)
                             (format "%02d:%02d" clock-minutes seconds)))
      (setq workout-time "PAUSED"))
    (when jackel-workout-rest-time
      (pcase-let* ((`(,rest-start . ,rest-target) jackel-workout-rest-time)
                   (seconds-passed (- (floor (float-time)) rest-start)))
        (if rest-target
            (let* ((bar-width 20)
                   (completed-percentage (/ (float seconds-passed) rest-target))
                   (fill-bar-ct (min (floor (* completed-percentage bar-width)) bar-width))
                   (blank-bar-ct (- bar-width fill-bar-ct))
                   (fill-str (make-string fill-bar-ct ?#))
                   (blank-str (make-string blank-bar-ct ?\s)))
              (when (> completed-percentage 1.0)
                (when (= (mod seconds-passed 2) 0)
                  (setq fill-str (propertize fill-str 'face 'error))))
              (setq rest-time (format "[%s%s] %ds/%ds" fill-str blank-str seconds-passed rest-target)))
          (let* ((mins (/ seconds-passed 60))
                 (secs (mod seconds-passed 60)))
            (if (> mins 0)
                (setq rest-time (format "REST: %d:%02d" mins secs))
              (setq rest-time (format "REST: %ds" secs)))))))
    (format "%s   %s   %s"
            jackel-active-workout-name
            workout-time
            rest-time)))

(defun jackel-workout-update-handler ()
  "Handler function for the timer `jackel-workout-mode-timer'.
Responsible for updating `header-line-format'."
  (setq-local header-line-format (jackel--header-line)))

(define-minor-mode jackel-workout-mode
  "Toggle `jackel-workout-mode'.
`jackel-workout-mode' provides an easy way to input workout data
by adding certain keybindings and automatically tracking certain changes."
  :global nil
  (when (timerp jackel-workout-mode-timer)
    (cancel-timer jackel-workout-mode-timer))
  (if (not jackel-workout-mode)
      (progn
        (setq header-line-format nil))
    (setq header-line-format (or jackel-active-workout-name ""))
    (setq jackel-workout-mode-timer (run-at-time nil 0.5 #'jackel-workout-update-handler))))


(defun jackel-new-workout (routine-headline)
  "Create a new workout set for selected ROUTINE-HEADLINE."
  (interactive (list (jackel-read-routine)))
  (jackel--capture-new-workout routine-headline))

(defun jackel-start-workout ()
  "Start or continue the workout at the current point."
  (interactive)
  (jackel--set-workout-to-active))

(defun jackel-new-workout-and-start (routine-headline)
  "Cerate new instance of ROUTINE-HEADLINE and start it."
  (interactive (list (jackel-read-routine)))
  (jackel--capture-new-workout routine-headline)
  (jackel--set-workout-to-active))

(defun jackel-workout-pause-start-session ()
  "Start/stop the org-clock assocciated with the current workout."
  (interactive)
  (if org-clock-current-task
      (org-clock-out)
    (save-excursion
      (goto-char jackel-workout-heading-marker)
      (org-clock-in))))

(defun jackel-workout-quit ()
  "Quit the current workout."
  (interactive)
  (when org-clock-current-task
    (org-clock-out))
  (jackel-workout-mode 0)
  (widen)
  (save-buffer))

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

;; (defun jackel-browse-exercises (exercise-elt)
;;   "Display the exercise page for given exercise org element EXERCISE-ELT."
;;   (interactive
;;    (list (jackel--completing-read-elements (jackel-exercise-elements))))
;;   (jackel-display-exercise exercise-elt))

(provide 'jackel)
;;; jackel.el ends here
