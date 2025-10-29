;;; org-fit.el --- Org-Fit is an org-mode based workout trakcer -*- lexical-binding: t -*-

;; Author: Zachary Romero
;; Version: 0.1.1
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/zkry/org-fit
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

;; Org-Fit is a org-mode based workout tracker.

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-clock)
(require 'org-capture)

(require 'seq)

(defgroup org-fit nil
  "Emacs Search Tool Aggregator."
  :prefix "org-fit"
  :group 'applications)

(defcustom org-fit-default-unit :kg
  "Default unit to log in if not specified."
  :type '(choice (const :kg) (const :lb)))

(defcustom org-fit-warmup-program
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

(defcustom org-fit-1rm-function #'org-fit--1rm-brzycki
  "Default 1rm calculationf function to use.
This should be a function that takes two arguments, a weight and rep count."
  :type 'function)


;;; Variables and constants

;; Properties

(defconst org-fit-autorest-property-name "ORG_FIT_AUTOREST"
  "The name of the property which defines how long rest intervals should be.")
(defconst org-fit-exercise-type-property-name "ORG_FIT_EXERCISE_TYPE"
  "Name of the property which defines how an exercise is to be recorded.
It can be any of the following strings: \"weight+reps\", \"distance+time\",
\"reps\", \"time\", or \"weight+time\".

By default, exercises are assumed to be of the type `(weight reps)'.")

(defconst org-fit-exercise-muscle-group-property-name "ORG_FIT_MUSCLE_GROUP"
  "Name of property which stores the musclegroup(s) that the exercise is for.
This should be a comma seperated list of items.")

(defconst org-fit-exercise-replaced-from-property-name "ORG_FIT_REPLACED_FROM"
  "Name of property which stores the musclegroup(s) that the exercise is for.
This should be a comma seperated list of items.")

;; System-heading related

(defvar org-fit-exercises-heading "Exercises"
  "Top-level heading for which the directory of exercises are located.")

(defvar org-fit-routines-heading "Routines"
  "Top-level heading for which the directory of exercise routines are located.")

(defvar org-fit-workouts-heading "Workouts"
  "Top-level heading for which the directory of workouts are located.")

;; Table-related

(defconst org-fit-column-name-weight "weight"
  "The column name for recording a weight.")
(defconst org-fit-column-name-reps "reps"
  "The column name for recording a rep count.")
(defconst org-fit-column-name-distance "distance"
  "The column name for recording a distance.")
(defconst org-fit-column-name-time "time"
  "The column name for recording a time.")
(defconst org-fit-column-name-notes "notes"
  "The column name for recording a time.")

(defconst org-fit-set-labels '("warmup" "failure" "drop")
  "List of labels that can apply to a given set.")

;; Workout-mode related

(defvar org-fit-workout-heading-marker nil
  "Variable to stor the top-level heading position.
For example, used to quickly find the item needed to clock in/out of.")

(defvar-local org-fit-fold-setting 'hide-other
  "Stores the auto-folding preference for org-fit-workout-mode.
It can be either the symbols `hide-other' so on only the current
set/superset is shown or all, to show all exercises.")
;; TODO: add defcustom  of org-fit-default-fold-setting, or document setting default value.

(defvar org-fit-insert-reference-point nil
  "Variable storing the spot where input was last run.
This is used to auto-clear fields when inputing a number.")

(defvar org-fit-workout-mode-timer nil
  "Timer updating various UI of the current active session.")

(defvar org-fit-active-workout-name nil
  "Variable to store the name of the current active routine.")

(defvar org-fit-workout-rest-time-history nil
  "Stores a stack of rest time histories.")

(defvar org-fit-workout-rest-time nil
  "Cons of the float time when the rest timer started and the target rest time.
If nil, the rest timer isn't active.")

(defvar org-fit-workout-new-data-entered nil
  "Non-nil if the user has started to edit a blank field.")

(defvar org-fit-rest-timer-over-hook '(org-fit--rest-over-beep)
  "Functions that run when the rest timer is over.")



;;; Org document structure

(defcustom org-fit-exercise-routine-files '()
  "List if files containing exercises and routines.
All files in this list will be scanned in the following manner:

- Any heading with the property \"ORG_FIT_EXERCISE_TYPE\" or
  \"ORG_FIT_MUSCLE_GROUP\" will be understood to be an exercise
  definition.

- Any heading with a non-empty \"ORG_FIT_ROUTINE\" property will
  be understood to be a workout routine.

While you may save workouts in any of the files listed here, they
can be stored separately."
  :type '(repeat file))

;; (setq org-fit-exercise-routine-files '("/Users/zacharyromero/org-workspace/apps/routines.org" "/Users/zacharyromero/dev/emacs/org-fit/exercises.org"))

(defcustom org-fit-workout-capture-templates '()
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

;; (setq org-fit-workout-capture-templates '(("Default" (file "/Users/zacharyromero/org-workspace/apps/workout.org"))))

(defun org-fit--elt-exercise-p (elt)
  "Return non-nil if ELT is an exercise element."
  (or (org-entry-get (org-element-property :begin elt) "ORG_FIT_MUSCLE_GROUP")
      (org-entry-get (org-element-property :begin elt) "ORG_FIT_EXERCISE_TYPE")))

(defun org-fit-extract-exercises ()
  "Return a list of available exercises."
  (let* ((exercises))
    (dolist (file org-fit-exercise-routine-files)
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)
        (let* ((parse-tree (org-element-parse-buffer)))
          (org-element-map parse-tree 'headline
            (lambda (hl)
              (when (or (org-entry-get (org-element-property :begin hl) "ORG_FIT_MUSCLE_GROUP")
                        (org-entry-get (org-element-property :begin hl) "ORG_FIT_EXERCISE_TYPE"))
                (push (org-element-property :raw-value hl) exercises)))))))
    (nreverse exercises)))

(defun org-fit-extract-routines ()
  "Return a list of available routines."
  (let* ((exercises (org-fit-extract-exercises)) ;; TODO - to hashmap?? ;; TODO - error if no exercises
         (routines)
         (case-fold-search t))
    (dolist (file org-fit-exercise-routine-files)
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)
        (let* ((parse-tree (org-element-parse-buffer)))
          (org-element-map parse-tree 'headline
            (lambda (hl)
              (when (org-entry-get (org-element-property :begin hl) "ORG_FIT_ROUTINE")
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

(defun org-fit-read-exercise ()
  "Prompt user to select a known exercise."
  (let* ((exercises (org-fit-extract-exercises)))
    (completing-read "Exercise: " exercises nil t nil 'org-fit-read-exercise)))

(defun org-fit-read-routine ()
  "Prompt user to select a known routine."
  (let* ((routines (org-fit-extract-routines))
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
    (completing-read "Routine: " routines nil t nil 'org-fit-read-routine)))

(defun org-fit--get-exercise-by-name (name)
  "Return the string of contents of exercise with NAME."
  (catch 'found
    (dolist (file org-fit-exercise-routine-files)
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)
        (while (search-forward name nil t)
          (let* ((hl (org-element-at-point)))
            (when (and (org-fit--elt-exercise-p hl)
                       (string-equal (org-element-property :raw-value hl) name))
              (throw 'found
                     (concat
                      (buffer-substring-no-properties (org-element-property :begin hl) (org-element-property :end hl)) "\n")))))))))

(defun org-fit--get-exercise-type (exercise-name)
  "Return the exercise type of exercise name EXERCISE-NAME."
  (let* ((exercise-data (org-fit--get-exercise-by-name exercise-name)))
    (with-temp-buffer
      (insert exercise-data)
      (org-mode)
      (org-fit--string-to-exercise-type (or (org-entry-get (point-min) "ORG_FIT_EXERCISE_TYPE") "weight+reps")))))

(defun org-fit--get-routine-by-name (name)
  "Return the string of contents of the routine with NAME."
  (catch 'found
    (dolist (file org-fit-exercise-routine-files)
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)
        (let* ((parse-tree (org-element-parse-buffer)))
          (org-element-map parse-tree 'headline
            (lambda (hl)
              (when (and (org-entry-get (org-element-property :begin hl) "ORG_FIT_ROUTINE")
                         (string-equal (org-element-property :raw-value hl) name))
                (throw 'found
                       (concat
                        (buffer-substring-no-properties (org-element-property :begin hl) (org-element-property :end hl)) "\n"))))))))))



;;; Units helper functions

(defun org-fit--parse-duration (str)
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

(defun org-fit--parse-weight (str)
  "Parse a weight string STR into a cons cell of (AMOUNT . UNIT)."
  (when (string-match "[0-9]+\\(\\.[0-9]+\\)?" str)
    (let* ((number (string-to-number (match-string 0 str)))
           (unit (and (string-match (regexp-opt '("kg" "lb" "lb")) str)
                      (match-string 0 str))))
      (if unit
          (cons number
                (pcase unit
                  ("kg" :kg)
                  ("lb" :lb)))
        (cons number org-fit-default-unit)))))

(defun org-fit--parse (str type)
  (pcase type
    ('reps (string-to-number str))
    ('weight (org-fit--parse-weight str))
    ('time (org-fit--parse-duration str))
    ('distance (error "Not implemented"))
    ('notes str)))

(defun org-fit--read-weight (&optional prompt)
  "Prompt the user to input a weight.
If PROMPT is a string use that as the prompt."
  (let ((weight (read-number (format "%s (%s): " (or prompt "Weight") org-fit-default-unit))))
    (cons weight org-fit-default-unit)))

(defun org-fit--weight-to-string (weight)
  "Return a string representing WEIGHT."
  (let*  ((clean-float-func (lambda (x)
                              (if (or (integerp x) (and (floatp x) (= (mod x 1) 0)))
                                  (floor x)
                                (format "%0.1f" x))))
          (unit-to-string '((:kg . "kg")
                            (:lb . "lb"))))
    (pcase weight
      (`(,amt . ,unit)
       (format "%s%s" (funcall clean-float-func amt) (alist-get unit unit-to-string)))
      ((cl-type null) "")
      (amt
       (format "%s%s" (funcall clean-float-func amt) (alist-get org-fit-default-unit unit-to-string))))))

(defun org-fit--value-to-string (val)
  "Return a string representing VAL.
Value can be a united number or a plain number."
  (cond
   ((numberp val) (format "%s" val))
   ((consp val)
    ;; TODO - this will need to change when durations and time are added.
    (org-fit--weight-to-string val))))

(defun org-fit--round-to-nearest (weight nearest)
  "Round WEIGHT to the closest multiple of NEAREST."
  (cond
   ((consp weight)
    (pcase-let* ((`(,amt . ,unit) weight))
      (cons
       (* (round (/ amt nearest) ) nearest)
       unit)))
   ((numberp weight)
    (* (round (/ weight nearest) ) nearest))))

(defun org-fit--* (a b)
  "Multiply a weight A by a scalar amount B.
Arguments may be reversed."
  (cond
   ((or (null a) (null b))
    nil)
   ((and (consp a) (consp b))
    (pcase-let ((`(,scalar1 . ,unit1) a)
                (`(,scalar2 . ,unit2) b))
      (cons (* scalar1 scalar2) `(* ,unit1 ,unit2))))
   ((consp b)
    (org-fit--* b a))
   ((and (numberp a) (numberp b))
    (* a b))
   (t
    (pcase-let ((`(,weight . ,unit) a))
      (cons (* weight b) unit)))))

(defun org-fit--convert (val to-type)
  (let* ((raw-val (car val)))
    (cons
     (pcase (list (cdr val) to-type)
       (`(:kg :lb) (* raw-val 2.20462))
       (`(:lb :kg) (* raw-val 0.453592)))
     to-type)))

(defun org-fit--reduce (op &rest items)
  "Multiply a weight A by a scalar amount B.
Arguments may be reversed."
  (seq-reduce
   (lambda (acc elt)
     (cond
      ((null acc)
       elt)
      ((null elt)
       acc)
      ((and (consp acc) (consp elt))
       (if (equal (cdr acc) (cdr elt))
           (cons (funcall op (car acc) (car elt)) (cdr acc))
         (let ((converted-elt (org-fit--convert elt (cdr acc))))
           (cons (funcall op (car acc) (car converted-elt)) (cdr acc)))))
      ((or (consp elt) (consp acc))
       (error "enable to add values of two different types"))
      (t (funcall + acc elt))))
   (cdr items)
   (car items)))

(defun org-fit--+ (&rest items)
  "Multiply a weight A by a scalar amount B.
Arguments may be reversed."
  (apply #'org-fit--reduce (append '(+) items)))

(defun org-fit--max (&rest items)
  "Multiply a weight A by a scalar amount B.
Arguments may be reversed."
  (apply #'org-fit--reduce (append '(max) items)))

(defun org-fit--move-up-to-workout ()
  "Move the point up to the top-level definition of a workout."
  (catch 'done
    (while t
      (let ((at-elt (org-element-at-point)))
        (when (and (eql (org-element-type at-elt) 'headline)
                   (alist-get "ORG_FIT_WORKOUT_ROUTINE" (org-entry-properties) nil nil 'equal))
          (throw 'done nil))
        (org-up-element)))))

(defun org-fit-table-to-lisp (&optional txt)
  "Convert a table to a list of plists representing the sets of an exercise."
  (let* ((data (org-table-to-lisp txt))
         (col-types (seq-map #'org-fit--column-name-to-type (car data))))
    (seq-map (lambda (row)
               (apply #'append
                (seq-mapn
                 (lambda (str type)
                   (cond
                    ((eql type 'notes)
                     (let* ((target (org-fit--extract-table-from-note str)))
                       (list 'notes str 'target target)))
                    (t (list type (org-fit--parse str type)))))
                 row col-types)))
             (cddr data))))

(defun org-fit-row-to-lisp ()
  "Return a list representation of the current row."
  (let* ((line (org-table-current-line))
         (col1 (org-fit--column-name-to-type (org-table-get 1 1)))
         (col1-val (org-fit--parse (org-table-get line 1) col1))
         (col2 (org-fit--column-name-to-type (org-table-get 1 2)))
         (col2-val (org-fit--parse (org-table-get line 2) col2))
         (col3 (org-fit--column-name-to-type (org-table-get 1 3)))
         (ret (list col1 col1-val
                    col2 col2-val)))
    (when col3
      (setq ret (append ret (list col3 (org-fit--parse (org-table-get line 3) col3)))))
    (when-let* ((note (plist-get ret 'notes))
                (target (org-fit--extract-table-from-note note)))
      (setq ret (append ret (list 'target target))))
    ret))

(defun org-fit-put-row (col-type val-str)
  "Write VAL-STR to the column of type COL-TYPE."
  (let* ((col-no (cl-loop for col from 1 to 3
                          if (eql col-type (org-fit--column-name-to-type (org-table-get 1 col)))
                          return col)))
    (org-table-put (org-table-current-line) col-no val-str t)))


;;; Exercises

(defun org-fit--exercise-body (exercise-name)
  "Find and return body of definition for EXERCISE-NAME."
  (catch 'done
    (dolist (file org-fit-exercise-routine-files)
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)
        (goto-char (point-min))
        (let ((case-fold-search t))
          (while (search-forward exercise-name nil t)
            (let* ((hl (org-element-at-point)))
              (when (equal (org-element-property :raw-value hl) exercise-name)
                (goto-char (org-element-property :contents-begin hl))
                (forward-line 0)
                (while (looking-at-p " *:")
                  (forward-line 1))
                (throw 'done
                       (buffer-substring-no-properties (point)
                                                       (org-element-property :contents-end hl)))))))))))


(defun org-fit--string-to-exercise-type (str)
  "Given an string STR return list of input column types."
  ;; "weight+reps\", \"distance+time\", \"reps\", \"time\", or \"weight+time\"
  (pcase str
    ("weight+reps" '(weight reps))
    ("distance+time" '(time distance))
    ("reps" '(reps))
    ("time" '(time))
    ("weight+time" '(weight time))))


;;; Routines

(defun org-fit--column-type-to-name (type)
  "Return the column name of a given column TYPE."
  (pcase type
    ('weight org-fit-column-name-weight)
    ('reps org-fit-column-name-reps)
    ('distance org-fit-column-name-distance)
    ('time org-fit-column-name-time)
    ('notes org-fit-column-name-notes)))

(defun org-fit--column-name-to-type (name)
  "Return the column type symbol of a given column NAME."
  (let ((name (downcase name)))
    (cond
     ((equal name org-fit-column-name-weight) 'weight)
     ((equal name org-fit-column-name-reps) 'reps)
     ((equal name org-fit-column-name-distance) 'distance)
     ((equal name org-fit-column-name-time) 'time)
     ((equal name org-fit-column-name-notes) 'notes))))

(defun org-fit--insert-blank-exercise-table (type)
  "Insert a blank exercise table according to TYPE."
  (insert "| ")
  (dolist (type-elt type)
    (pcase type-elt
      ('weight
       (insert org-fit-column-name-weight))
      ('reps
       (insert org-fit-column-name-reps))
      ('time
       (insert org-fit-column-name-time))
      ('distance
       (insert org-fit-column-name-distance)))
    (insert " | "))
  (insert org-fit-column-name-notes " |\n")
  (if (= (length type) 2)
      (insert "|-+-+-|\n| | | |\n")
    (insert "|-+-|\n| | |\n"))
  (org-table-align)
  (org-table-goto-line 2)
  (org-table-goto-column 1))

;;; Interactive Commands (for routines):

(defun org-fit-routine-insert-exercise (exercise-name)
  "Insert a known exercise EXERCISE-NAME after the current point."
  (interactive (list (org-fit-read-exercise)))
  (org-fit-workout-add-exercise exercise-name))



;;; History

(defun org-fit--workout-files ()
  "Return list of files which contain workouts."
  (let ((files))
    (pcase-dolist (`(,_desc (,type ,f . ,_rest)) org-fit-workout-capture-templates)
      (when (member type '(file file+headline file+olp))
        (push f files)))
    (nreverse files)))

(defun org-fit--scan-past-workout-routines (routine-name)
  "Scan all workouts for routines of ROUTINE-NAME."
  (let ((case-fold-search t)
        (results))
    (dolist (workout-file (org-fit--workout-files))
      (with-temp-buffer
        (insert-file-contents workout-file)
        (org-mode)
        (goto-char (point-min))
        (while (search-forward-regexp (concat ":ORG_FIT_WORKOUT_ROUTINE:.*"
                                              (regexp-quote routine-name))
                                      nil t)
          (let ((date)
                (routine-text))
            (save-excursion
              (org-fit--move-up-to-workout)
              (let* ((heading (org-get-heading)))
                (save-match-data
                  (string-match org-element--timestamp-regexp heading)
                  (setq date (match-string 1 heading)))))
            (save-excursion
              (save-restriction
                (org-narrow-to-subtree)
                (setq routine-text (buffer-string))))
            (push `((date . ,date)
                    (routine . ,routine-text))
                  results)))))
    (seq-sort-by (lambda (elt) (alist-get 'date elt))
                 #'string>
                 results)))

(defun org-fit--scan-past-workout-exercises (exercise-name)
  "Scan all workouts for exercises of EXERCISE-NAME."
  (let ((case-fold-search t)
        (results))
    (dolist (workout-file (org-fit--workout-files))
      (with-temp-buffer
        (insert-file-contents workout-file)
        (org-mode)
        (goto-char (point-min))
        (while (search-forward-regexp (concat "^\\**[ a-zA-Z]+" exercise-name) nil t)
          (let ((date)
                (exercise-table))
            (save-excursion
              (org-fit--move-up-to-workout)
              (let* ((heading (org-get-heading)))
                (save-match-data
                  (string-match org-element--timestamp-regexp heading)
                  (setq date (match-string 1 heading)))))
            (save-excursion
              (save-restriction
                (org-narrow-to-subtree)
                (when (search-forward-regexp org-table-line-regexp nil t)
                  (setq exercise-table (org-fit-table-to-lisp)))))
            (when exercise-table
              (push `((date . ,date)
                      (data . ,exercise-table))
                    results))))))
    (seq-sort-by (lambda (elt) (alist-get 'date elt))
                 #'string>
                 results)))



;;; Summaries and Statistics

;; The most popular formula is the formula from Matt Brzycki, which is weight divided by ( 1.0278 – 0.0278 × reps ).

;; Epley’s formula is the weight multiplied by (1 + 0.0333 × reps).

;; Lander’s formula is (100 × weight ) / (101.3 – 2.67123 × reps).

;; Lombardi’s formula is weight × reps ^ 0.1.

(defun org-fit--table-volume (table-data)
  "Calculate the volume from TABLE-DATA."
  (apply
   #'org-fit--+
   (seq-map
    (lambda (row)
      (let ((weight (plist-get row 'weight))
            (reps (plist-get row 'reps)))
        (when (and weight reps)
          (org-fit--* weight reps))))
    table-data)))

(defun org-fit--table-1rm-row (table-data)
  "Return row data of best 1rm from TABLE-DATA."
  (let ((1rm-to-row (make-hash-table :test #'equal))
        (best-1rm))
    (setq best-1rm
          (apply
           #'org-fit--max
           (seq-map
            (lambda (row)
              (let* ((weight (plist-get row 'weight))
                     (reps (plist-get row 'reps)))
                (when (and weight reps)
                  (let* ((1rm (funcall org-fit-1rm-function weight reps)))
                    (puthash 1rm (list weight reps) 1rm-to-row)
                    1rm))))
            table-data)))
    (gethash best-1rm 1rm-to-row)))

(defun org-fit--table-1rm (table-data)
  "Calculate the best 1RM from TABLE-DATA."
  (apply
   #'org-fit--max
   (seq-map
    (lambda (row)
      (let ((weight (plist-get row 'weight))
            (reps (plist-get row 'reps)))
        (funcall org-fit-1rm-function weight reps)))
    table-data)))

(defun org-fit--table-best-set-volume (table-data)
  "Calculate the best 1RM from TABLE-DATA."
  (apply
   #'org-fit--max
   (seq-map
    (lambda (row)
      (let ((weight (plist-get row 'weight))
            (reps (plist-get row 'reps)))
        (org-fit--* weight reps)))
    table-data)))

(defun org-fit--1rm-brzycki (weight reps)
  (org-fit--* weight (/ 1.0 (- 1.0278 (* 0.0278 reps)))))

(defun org-fit--1rm-epley (weight reps)
  (if (= 1 reps)
      weight
    (org-fit--* weight (+ 1.0 (/ reps 30.0)))))

(defun org-fit--1rm-lander (weight reps)
  (org-fit--* (org-fit--* 100 weight)
             (/ 1.0 (- 101.3 (* 2.67123 reps)))))



;;; Workouts

(defun org-fit--get-rest-time ()
  "Return the specified rest time at the current point."
  (let* ((rest-str (org-entry-get (point) org-fit-autorest-property-name)))
    (when rest-str
      (org-fit--parse-duration rest-str))))

(defun org-fit--make-note (val type)
  "Return string representation of VAL having meaning TYPE."
  (unless (string-blank-p val)
    (pcase type
      ('reps (format "x%s" val))
      ('weight (org-fit--weight-to-string (org-fit--parse-weight val))))))

(defun org-fit--remove-plan-note (note)
  "Remotve workout plan section of NOTE if exits.
Workout plan is the first semicolon separated section of a note
that contains direction what should be filled in the table."

  (let* ((parts (string-split note ";" )))
    (if (org-fit--plan-note-p  (car parts))
        (string-join (cdr parts) "")
      note)))

(defun org-fit--routine-set-to-note (line)
  "Blank out table LINE number and store it in notes."
  (let* ((col1 (org-fit--column-name-to-type (org-table-get 1 1)))
         (col2 (org-fit--column-name-to-type (org-table-get 1 2)))
         (_col3 (org-fit--column-name-to-type (org-table-get 1 3))))
    (let* ((two-input-table-p (equal (org-fit--table-current-column-type 3) 'notes)))
      (if two-input-table-p
          (let* ((note1 (org-fit--make-note (org-table-get line 1) col1))
               (note2 (org-fit--make-note (org-table-get line 2) col2))
               (note (concat note1 " " note2))
               (old-note (org-fit--remove-plan-note (org-table-get line 3)))
               (new-note (if (string-blank-p old-note)
                             note
                           (concat note "; " old-note))))
          (org-table-put line 1 "")
          (org-table-put line 2 "")
          (org-table-put line 3 new-note))
        (let* ((note (org-fit--make-note (org-table-get line 1) col1))
                 (old-note (org-fit--remove-plan-note (org-table-get line 2)))
                 (new-note (if (string-blank-p old-note)
                               note
                             (concat note "; " old-note))))
            (org-table-put line 2 new-note)
            (org-table-put line 1 ""))))))

(defun org-fit--routine-sets-to-notes (routine-str)
  "Move all inputted data to notes section for ROUTINE-STR.
Routine definitions are meant to store the templates of workouts
so data shoudln't be entered in the non-last columns for
routines.  If this is done, this function is to take that data
and move it to the notes section in a way that is understandable
by the command `org-fit-fill-in'."
  (with-temp-buffer
    (insert routine-str)
    (org-mode)
    (goto-char (point-min))
    (while (search-forward (concat "| " org-fit-column-name-notes) nil t)
        (org-table-analyze)
        (let* ((line-ct (- (length org-table-dlines) 2)))
          (cl-loop for line from 2 to (1+ line-ct)
                   do
                   (org-fit--routine-set-to-note line)))
        (org-table-align))
    (buffer-string)))

(defun org-fit--routine-string-to-workout (routine)
  "Convert a ROUTINE string to a workout entry."
  (let ((case-fold-search t))
    (with-temp-buffer
      (insert routine)
      (org-mode)
      (goto-char (point-min))
      ;; Remove date in header
      ;; Remove DONE status
      (save-excursion
        (while (re-search-forward " +DONE" nil t)
          (replace-match "")))
      (save-excursion
        (while (re-search-forward ":LOGBOOK:" nil t)
          (let ((start (pos-bol)))
            (when (re-search-forward ":END:" nil t)
              (delete-region start (pos-eol))))))
      (save-excursion
        (while (search-forward ":org-fit_routine:" nil t)
          (delete-region (1- (pos-bol)) (pos-eol))))
      (while (not (looking-at org-heading-regexp))
        (forward-line t))
      (save-excursion
        (when (search-forward-regexp org-ts-regexp-both (pos-eol) t)
          (replace-match "")))
      (save-excursion
        (let* ((hl-elt (org-element-at-point))
               (hl-name (org-element-property :raw-value hl-elt)))
          (search-forward hl-name)
          (replace-match (concat hl-name " " (format-time-string "[%Y-%m-%d %a]")))
          (org-set-property "ORG_FIT_WORKOUT_ROUTINE" hl-name)))
      (buffer-string))))

(defun org-fit--capture-new-workout (routine-string)
  "Create new workout entry according to contents of ROUTINE-STRING."
  ;;; org-capture-goto-target
  (let* ((selected-template
          (if (= (length org-fit-workout-capture-templates) 1)
              (car org-fit-workout-capture-templates)
            (let ((selected (completing-read
                             "Workout capture: "
                             (seq-map #'car org-fit-workout-capture-templates)
                             nil
                             t)))
              (seq-find (pcase-lambda (`(,desc ,_))
                          (equal desc selected))
                        org-fit-workout-capture-templates))))
         (workout-contents (org-fit--routine-sets-to-notes
                            (org-fit--routine-string-to-workout
                             routine-string)))
         ;; TODO: if more than one, prompt user
         (org-capture-entry `("t" "" entry ,(cadr selected-template)
                              ,workout-contents
                              :immediate-finish t)))
    (org-capture)
    (org-capture '(16))))

(defun org-fit--set-workout-to-active ()
  "Find the workout of the point and make it active, enabling org-fit-workout-mode."
  ;; TODO: catch error and print nicer message
  (org-fit--move-up-to-workout)
  (let* ((workout-name (alist-get "ORG_FIT_WORKOUT_ROUTINE" (org-entry-properties) nil nil 'equal))
         (workout-marker (make-marker)))
    (org-clock-in)
    (set-marker workout-marker (point))
    (setq org-fit-workout-heading-marker workout-marker)
    (org-narrow-to-subtree)
    (setq org-fit-active-workout-name workout-name)
    (org-fit-workout-mode 1)))

(defvar org-fit-workout-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)

    (keymap-set map "?" #'org-fit-workout-help)

    ;; Movement and Viewing
    (keymap-set map "<tab>" #'org-fit-workout-next-empty-field)
    (keymap-set map "<backtab>" #'org-fit-workout-previous-empty-field)

    (keymap-set map "<up>" #'org-fit-workout-up-field)
    (keymap-set map "<down>" #'org-fit-workout-down-field)
    (keymap-set map "<left>" #'org-fit-workout-left-field)
    (keymap-set map "<right>" #'org-fit-workout-right-field)

    (keymap-set map "p" #'org-fit-workout-up-field)
    (keymap-set map "n" #'org-fit-workout-down-field)
    (keymap-set map "f" #'org-fit-workout-left-field)
    (keymap-set map "b" #'org-fit-workout-right-field)

    (keymap-set map "<mouse-1>" #'org-fit-set-point)

    ;; Set Management
    (keymap-set map "+" #'org-fit-workout-add-set)
    (keymap-set map "-" #'org-fit-workout-remove-set)
    (keymap-set map "'" #'org-fit-workout-add-note)
    (keymap-set map "W" #'org-fit-workout-add-warmup-set)
    (keymap-set map "e" #'org-table-edit-field)
    (keymap-set map "x" #'org-table-blank-field)
    (keymap-set map ">" #'org-fit-stash-set)
    (keymap-set map "t" #'org-fit-workout-set-type)

    ;; Exercise Management
    (keymap-set map "E" #'org-fit-workout-add-exercise)
    (keymap-set map "D" #'org-fit-workout-remove-exercise)
    (keymap-set map "S" #'org-fit-workout-swap-exercise)
    (keymap-set map "i" #'org-fit-workout-info-dwim)
    (keymap-set map "v" #'org-fit-workout-toggle-view)

    (keymap-set map "S-<up>" #'org-move-subtree-up)
    (keymap-set map "S-<down>" #'org-move-subtree-down)

    ;; Data Entry
    (keymap-set map "u" #'org-fit-workout-convert-unit)
    (keymap-set map "<spc>" #'org-fit-fill-in)
    (keymap-set map "<SPC>" #'org-fit-fill-in)
    (keymap-set map "\"" #'org-fit-workout-ditto) ;; something more native?
    (keymap-set map "<volume-down>" #'org-fit-fill-in-and-next-empty-field)
    (keymap-set map "<volume-up>" #'org-fit-edit-and-next-empty-field)

    (dotimes (i 10)
      (keymap-set map (string (+ ?0 i)) #'org-fit-self-insert-command))
    (keymap-set map "." #'org-fit-self-insert-command)

    ;; Session Management
    (keymap-set map "r" #'org-fit-workout-rest)
    (keymap-set map "C-c r" #'org-fit-workout-rest-pop)
    (keymap-set map "R" #'org-fit-workout-rest)
    (keymap-set map "P" #'org-fit-workout-pause-start-session)
    (keymap-set map "Q" #'org-fit-workout-quit)
    (keymap-set map "U" #'undo)

    map)
  "Mode map for `org-fit-workout-mode'.")

(defun org-fit--table-current-column-type (&optional column)
  "Return the string of the first row of the current column.
If COLUMN is a number, use it instead of the current column."
  (let ((current-column (or column (org-table-current-column))))
    (when (> current-column 0) ;; we are in a table
      (org-fit--column-name-to-type (org-table-get 1 current-column)))))

(defun org-fit--line-note (&optional input-section)
  "Return the note string for the current line.
If INPUT-SECTION is non-nil, get the first semicolon delimited text."
  ;; TODO: This could be done more stabily by checking for column names.
  (let* ((current-line (org-table-current-line))
         (two-input-table-p (equal (org-fit--table-current-column-type 3) 'notes))
         (col3-data (org-table-get current-line 3))
         (res (if two-input-table-p
                  col3-data
                (org-table-get current-line 2))))
    (if input-section
        (car (string-split res ";"))
      res)))

(defun org-fit--on-set-line ()
  "Return non-nil if the point is on a line for inputing set data."
  (and (org-at-table-p)
       (> (org-table-current-line) 1)))

(defun org-fit--target-weight-of-set (&optional note)
  "Return weight specification.
If NOTE is a string, use that instead of getting the current line's cell."
  (when (and (not note) (not (org-fit--on-set-line)))
    (error "No set on current line"))
  (let* ((note-text (or note (org-fit--line-note t))))
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
        (cons (string-to-number weight) org-fit-default-unit))))))

(defun org-fit--target-reps-of-set (&optional note)
  "Return weight specification.
If NOTE is a string, use that instead of getting the current line's cell."
  (when (and (not note) (not (org-fit--on-set-line)))
    (error "No set on current line"))
  (let* ((note-text (or note (org-fit--line-note t))))
    (cond
     ((string-match "x ?\\([1-9][0-9]*\\)" note-text)
      (let* ((reps (match-string 1 note-text)))
        (string-to-number reps))))))

(defun org-fit--plan-note-p (note)
  "Return non-nil if NOTE is a plan.
A plan is a special note that indicates how the user intends to complete a set."
  ;; TODO this is a hackey way to determine this but it seems like it
  ;; should work.
  (seq-every-p
   (lambda (part)
     (or (string= part "x")
         (string-match-p "[0-9]" part)))
   (string-split note " " t " ")))

;; TODO: time spec of MM:SS or HH:MM:SS or min s sec h hour second minute
;; TODO: distance spec of mi, km, m, ft, cm, or in.
(defun org-fit--extract-table-from-note (&optional note)
  "Return a preset value alist of notes column of current line."
  (when (> (org-table-current-line) 1)
    (let* ((maybe-reps (org-fit--target-reps-of-set note))
           (maybe-weight (org-fit--target-weight-of-set note))
           (res))
      (when maybe-reps
        (push (cons 'reps maybe-reps) res))
      (when maybe-weight
        (push (cons 'weight maybe-weight) res))
      (or res
          (save-excursion
            (org-table-goto-line (1- (org-table-current-line)))
            (org-fit--extract-table-from-note))))))

(defun org-fit--apply-fold-setting ()
  "Assuming buffer is unfolded, apply fold settings of `org-fit-fold-setting'."
  (pcase org-fit-fold-setting
    ('all nil)
    ('hide-other
     (let* ((pos (point)))
       (org-cycle-global 16)
       (org-reveal)
       (goto-char pos)))))

(defun org-fit--current-table-complete ()
  "Return non-nil if the current table at point is all filled in."
  (when (and (org-at-table-p))
    (thread-last (seq-drop (org-table-to-lisp) 2)
                 (seq-map (lambda (row) (butlast row 1)))
                 (flatten-list)
                 (seq-every-p (lambda (x) (not (string-blank-p x)))))))

(defun org-fit--process-table-changes ()
  "Process new table state e.g. marking exercise DONE or starting rest timer."
  (when org-fit-workout-new-data-entered
    ;; Auto-restart rest timer
    (when-let* ((two-column-input (and (org-at-table-p)
                                       (equal (org-fit--table-current-column-type 3) 'notes)))
                (complete (if two-column-input
                              (and (not (string-blank-p (org-table-get (org-table-current-line) 1)))
                                   (not (string-blank-p (org-table-get (org-table-current-line) 2))))
                            (not (string-blank-p (org-table-get (org-table-current-line) 1))))))
      (org-fit--start-rest-timer  (org-fit--get-rest-time)))

    ;; Mark headline as done


    (setq org-fit-workout-new-data-entered nil))
  (if (org-fit--current-table-complete)
      (save-excursion
        (org-back-to-heading t)
        (org-todo "DONE"))
    (save-excursion
      (org-back-to-heading t)
      (org-todo ""))))

(defmacro org-fit--with-workout-fold-settings (&rest body)
  "Run BODY with all headings unfolded.  After BODY apply fold settings."
  (declare (debug t) (indent 0))
  `(progn
     (org-fit--process-table-changes)
     (org-fold-show-all '(headings))
     ,@body
     (org-fit--apply-fold-setting)
     (org-table-align)))

(defun org-fit--input-column-types ()
  "Return list of downcased column names that contain performance data."
  (list 'time 'distance 'reps 'weight))

(defun org-fit--start-rest-timer (&optional target)
  "Set the rest timer.
If TARGET is an integer, set timer for TARGET seconds in the future."
  (setq org-fit-workout-rest-time (cons (floor (float-time)) target))
  (push org-fit-workout-rest-time org-fit-workout-rest-time-history))

(defun org-fit--add-rest-time (seconds)
  "Add SECONDS to the current rest time.
If no rest time is active, set timer for SECONDS in the future.
If the rest time is over the previous target, set a new target to
SECONDS after now."
  (pcase org-fit-workout-rest-time
    (`(,_start . nil)
     (setq org-fit-workout-rest-time (cons (floor (float-time)) seconds)))
    (`(,start ,duration ,done)
     (setq org-fit-workout-rest-time (list start (if duration (+ duration seconds) seconds) done)))
    (`(,start . ,duration)
     (setq org-fit-workout-rest-time (cons start (if duration (+ duration seconds) seconds))))
    ('nil
     (setq org-fit-workout-rest-time (cons (floor (float-time)) seconds)))))

(defun org-fit--stop-rest-timer ()
  "Clear rest timer, stopping it."
  (setq org-fit-workout-rest-time nil))


;; Workout commands:

(defun org-fit-workout-help ()
  "Display help message."
  (let* ((max-mini-window-height .75)
         (help-groups '(("Movement and Viewing"
                         (org-fit-workout-next-empty-field . "Move to next empty field")
                         (org-fit-workout-previous-empty-field . "Move to previous empty field")
                         (org-fit-workout-up-field . "Move to the field above current")
                         (org-fit-workout-down-field  . "Move to the field below current")
                         (org-fit-workout-left-field . "Move to the previous field")
                         (org-fit-workout-right-field . "Move to the next field"))
                        ("Set Management"
                         (org-fit-workout-add-set . "Insert set row")
                         (org-fit-workout-remove-set . "Remove set row")
                         (org-fit-workout-add-note . "Add note to current set")
                         (org-fit-workout-add-warmup-set . "Add warmup set. Repeat for multiple")
                         (org-table-edit-field . "Edit current field")
                         (org-table-blank-field . "Blank out field"))
                        ("Exercise Management"
                         (org-fit-workout-add-exercise . "Add new exercise")
                         (org-fit-workout-remove-exercise . "Remove the current exercise")
                         (org-fit-workout-swap-exercise . "Swap current exercise for new one")
                         (org-fit-workout-toggle-view . "Toggle visibility of exercises between show-all and show-curent")
                         (org-move-subtree-up . "Move exercise up")
                         (org-move-subtree-down . "Move exercise down"))
                        ("Data Entry"
                         (org-fit-fill-in . "Fill in based on exercise notes")
                         (org-fit-workout-ditto . "Copy values from previous set")
                         (org-fit-fill-in-and-next-empty-field . "Fill in based on notes and go to next empty")
                         (org-fit-edit-and-next-empty-field . "Manually set current and go to the next field"))
                        ("Session Management"
                         (org-fit-workout-rest . "Toggle rest timer. If over stop it. If no rest start it. If running, add 15s.")
                         (org-fit-workout-pause-start-session . "Pause/resume session timer.")
                         (org-fit-workout-quit . "Exit the current workout session."))))
         (help-str ""))
    (pcase-dolist (`(,heading . ,items ) help-groups)
      (setq help-str (concat help-str (propertize heading 'face '(:underline t)) ":\n") )
      (pcase-dolist (`(,cmd . ,description) items)
        (let* ((cmd-keys (seq-map
                          (lambda (str)
                            (propertize str 'face 'help-key-binding))
                          (seq-filter
                           (lambda (str)
                             (not (string-prefix-p "<menu" str)))
                           (seq-map #'key-description (where-is-internal cmd)))))
               (cmd-str (string-join cmd-keys ", ")))
          (when cmd-keys
            (setq help-str
                  (concat help-str
                          (format "%10s" (propertize cmd-str 'face 'help-key-binding))
                          " "
                          description "\n")))))
      (setq help-str (concat help-str "\n")))
    (let ((buf (get-buffer-create "*org-fit-workout-help*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert help-str)
          (special-mode))
        (goto-char (point-min)))
      (select-window (display-buffer buf)))))

(defun org-fit-workout-next-empty-field ()
  "Move the point to the next empty field in a workout session."
  (interactive)
  (org-fit--with-workout-fold-settings
    (let ((rest-time)
          (row-complete))
      (when-let* ((two-column-input (and (org-at-table-p)
                                         (equal (org-fit--table-current-column-type 3) 'notes)))
                  (complete (if two-column-input
                                    (and (not (string-blank-p (org-table-get (org-table-current-line) 1)))
                                         (not (string-blank-p (org-table-get (org-table-current-line) 2))))
                                  (not (string-blank-p (org-table-get (org-table-current-line) 1))))))
        (setq row-complete complete)
        (setq rest-time (org-fit--get-rest-time)))
      (let ((starting-point (point))
            (next-field-column-names (org-fit--input-column-types)))
        (catch 'done
          (while t
            (let ((next-field-exists (search-forward-regexp "|\\( \\) *|" nil t)))
              (unless next-field-exists
                (throw 'done nil))
              (if next-field-exists
                  (progn
                    (goto-char (match-end 1))
                    (when (member (org-fit--table-current-column-type) next-field-column-names)
                      (throw 'done (point))))
                (beep)
                (goto-char starting-point)
                (throw 'done (point)))))))
      (when row-complete
        (org-fit--start-rest-timer rest-time)))))

(defun org-fit-workout-previous-empty-field ()
  "Move the point to the previous empty field in a workout session."
  (interactive)
  (org-fit--with-workout-fold-settings
   (let ((starting-point (point))
         (next-field-column-names (org-fit--input-column-types)))
     (catch 'done
       (while t
         (let ((next-field-exists (search-backward-regexp "|\\( \\) *|" nil t)))
           (if next-field-exists
               (progn
                 (goto-char (match-end 1))
                 (when (member (org-fit--table-current-column-type) next-field-column-names)
                   (throw 'done nil)))
             (beep)
             (goto-char starting-point)
             (throw 'done nil))))))))

(defconst org-fit-workout-table-regex
  (concat "| " " *" org-fit-column-name-notes " *" " |")
  "Regex which can be searched for to navigate tables.")

(defun org-fit--previous-table ()
  "Move the point to the previous workout table."
  (search-backward-regexp org-fit-workout-table-regex nil t))

(defun org-fit--next-table ()
  "Move the point to the next workout table."
  (search-forward-regexp org-fit-workout-table-regex nil t))

(defun org-fit-workout-up-field ()
  "Move the next field above the current one."
  (interactive)
  (org-fit--with-workout-fold-settings
    (let ((case-fold-search t)
          (current-column (org-table-current-column)))
      (if (= current-column 0)
          (org-fit-workout-left-field)
        (let ((current-line (org-table-current-line)))
          (if (> current-line 2)
              (progn
                (org-table-goto-line (1- current-line))
                (org-table-goto-column current-column))
            ;; Go to previous table's last entry
            (org-fold-show-all)
            (org-table-goto-line 1)
            (forward-line -1)
            (when (org-fit--previous-table)
              (org-table-goto-line 9999)
              (org-table-goto-column current-column))))))))

(defun org-fit-workout-down-field ()
  "Move the next field below the current one."
  (interactive)
  (org-fit--with-workout-fold-settings
    (let ((case-fold-search t)
          (current-column (org-table-current-column)))
      (if (= current-column 0)
          (progn
            (org-fit--next-table)
            (org-table-goto-line 2)
            (org-table-goto-column 1))
        (let ((current-line (org-table-current-line)))
          (org-table-goto-line (1+ current-line))
          (org-table-goto-column current-column)
          (when (= (org-table-current-line) current-line)
            (when (org-fit--next-table)
              (org-table-goto-line 2)
              (org-table-goto-column current-column))))))))

(defun org-fit-workout-right-field ()
  "Move the next field below the current one."
  (interactive)
  (org-fit--with-workout-fold-settings
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
            (org-fit--next-table)
            (org-table-goto-line 2)
            (org-table-goto-column 1))
        (org-table-next-field)))))

(defun org-fit-workout-left-field ()
  "Move the next field below the current one."
  (interactive)
  (org-fit--with-workout-fold-settings
    (let ((case-fold-search t)
          (current-line (org-table-current-line))
          (current-column (org-table-current-column)))
      (if (and (<= current-line 2)
               (<= current-column 1))
          (progn
            (goto-char (org-table-begin))
            (org-fit--previous-table)
            (let ((last-line (save-excursion
                               (goto-char (- (org-table-end) 2))
                               (org-table-current-line)))
                  (last-col (save-excursion
                              (goto-char (- (org-table-end) 2))
                              (org-table-current-column))))
              (org-table-goto-line last-line)
              (org-table-goto-column last-col)))
        (org-table-previous-field)))))

(defun org-fit-workout-add-set ()
  "Add a set to the current routine."
  (interactive)
  (org-table-insert-row 2))

(defun org-fit-workout-remove-set ()
  "Remove the set on the current line."
  (interactive)
  (org-table-kill-row))

(defun org-fit-workout-add-note (note)
  "Add NOTE to the current set's note column."
  (interactive "sNote: ")
  (unless (org-fit--on-set-line)
    (user-error "No set on current line"))
  (let* ((current-line (org-table-current-line))
         (current-note (org-table-get current-line 3)) ;; TODO - make more general
         (new-note (if (string-blank-p current-note)
                       note
                     (concat current-note "; " note))))
    (org-table-put current-line 3 new-note t)))

(defun org-fit-workout-add-warmup-set ()
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
            (setq set-target-weight (org-fit--target-weight-of-set note))))
        (cl-incf line)))
    (org-table-goto-line 2)
    (org-table-goto-column 2)
    (let* ((set-target-weight (or set-target-weight
                                  (org-fit--read-weight "Warmup leading to weight")))
           (warmup-routine (seq-find
                            (lambda (program)
                              (= (length program) warmup-count))
                            org-fit-warmup-program)))
      (org-table-insert-row)
      (dotimes (i warmup-count)
        (pcase-let* ((`(,percent . ,reps) (nth i warmup-routine))
                     (line (+ 2 i))
                     (warmup-set-weight (and percent (org-fit--* set-target-weight percent))))
          (if reps
              (org-table-put line 1 (number-to-string reps))
            (org-table-put line 1 ""))
          (org-table-put line 3
                         (if warmup-set-weight
                             (concat (org-fit--weight-to-string
                                      (org-fit--round-to-nearest
                                       warmup-set-weight
                                       0.5)) ;; TODO: Make customizable
                                     "; warmup")
                           "warmup")
                         t))))))

(defun org-fit-stash-set ()
  "Blank the current set line and store it in a not on the current line."
  (interactive)
  (unless (org-at-table-p)
    (user-error "No table at point"))
  (org-fit--routine-set-to-note (org-table-current-line))
  (org-table-align))

(defun org-fit-workout-set-type (arg)
  "Set label of the curent set based on org-fit-set-labels.
If prefix ARG is provided, clear the set label."
  (interactive (list (prefix-numeric-value current-prefix-arg)))
  (unless (org-at-table-p)
    (user-error "No table at point"))
  (if (= 4 arg)
      (let* ((note (plist-get (org-fit-row-to-lisp) 'notes))
             (parts (string-split note ";" nil " *")))
        (org-fit-put-row 'notes
                      (string-join
                       (seq-remove
                        (lambda (part)
                          (seq-find (lambda (label)
                                      (string= label part))
                                    org-fit-set-labels))
                        parts)
                       "; ")))
    (let* ((label (completing-read "Lablel: " (cons "REMOVE" org-fit-set-labels) nil t))
           (note (plist-get (org-fit-row-to-lisp) 'notes))
           (parts (string-split note ";" nil " *"))
           (no-append nil)
           (replaced (string-join (seq-map
                                   (lambda (part)
                                     (if (seq-find (lambda (label)
                                                     (string= label part))
                                                   org-fit-set-labels)
                                         (progn
                                           (setq no-append t)
                                           label)
                                       part))
                                   parts)
                                  "; ")))
      (cond
       ((equal label "REMOVE")
        (org-fit-workout-set-type 4))
       (no-append
        (org-fit-put-row 'notes replaced))
       (t
        (org-fit-put-row 'notes (concat note "; " label)))))))`
(defun org-fit-workout-remove-exercise ()
  "Delete the current exercise in workout."
  (interactive)
  (org-mark-subtree)
  (delete-region (region-beginning) (region-end))
  (unless (org-fit-workout-next-empty-field)
    (org-fit-workout-previous-empty-field)))

(defun org-fit-workout-swap-exercise (exercise-name arg)
  "Replace current exercise with a different exercise named EXERCISE-NAME."
  (interactive (list (org-fit-read-exercise) (prefix-numeric-value current-prefix-arg)))
  (let ((prev-replaced (org-entry-get (point) org-fit-exercise-replaced-from-property-name))
        (current-val (org-get-heading t t t t)))
    (unless (equal exercise-name current-val)
      (unless prev-replaced
        (org-set-property org-fit-exercise-replaced-from-property-name current-val))
      (if (= arg 4)
          (org-fit--with-workout-fold-settings
           (let ((m (make-marker)))
             (set-marker m (point))
             (org-fit-workout-remove-exercise)
             (goto-char m)
             (forward-line -1)
             (org-fit-workout-add-exercise exercise-name)))
        (org-edit-headline exercise-name)))))

(defun org-fit-workout-toggle-view ()
  "Toggle view mode from viewing current set to viewing all."
  (interactive)
  (if (eql org-fit-fold-setting 'all)
      (progn
        (setq org-fit-fold-setting 'hide-other)
        (message "View set to hiding other exercises."))
    (setq org-fit-fold-setting 'all)
    (message "View set to showing all."))
  (org-fold-show-all '(headings))
  (org-fit--apply-fold-setting))

(defun org-fit-workout-info-dwim ()
  "Display info buffer regarding current position."
  (interactive)
  (let* ((on-routine-p (alist-get "ORG_FIT_WORKOUT_ROUTINE" (org-entry-properties) nil nil 'equal)))
    ;; TODO - should I add anything here for supersets?
    (unless on-routine-p
      (let* ((exercise-name (org-get-heading t t t t))
             (body (org-fit--exercise-body exercise-name))
             (history (org-fit--scan-past-workout-exercises exercise-name))
             (current (save-excursion
                        (save-restriction
                          (goto-char (point-min))
                          (search-forward-regexp "^|")
                          (org-fit-table-to-lisp))))
             (buf (get-buffer-create (format "*Exercise: %s*" exercise-name))))
        (with-current-buffer buf
          (erase-buffer)
          (insert exercise-name)
          (insert "\n")
          (when body
            (insert body)
            (insert "\n"))
          (let* ((best-set-volume (apply #'org-fit--max
                                         (seq-map
                                          (lambda (hist)
                                            (org-fit--table-best-set-volume (alist-get 'data hist)))
                                          history)))
                 (best-volume (apply #'org-fit--max
                                     (seq-map
                                      (lambda (hist)
                                        (org-fit--table-volume (alist-get 'data hist)))
                                      history)))
                 (best-1rm (apply #'org-fit--max
                                  (seq-map
                                   (lambda (hist)
                                     (org-fit--table-1rm (alist-get 'data hist)))
                                   history)))
                 (best-1rm-hist (seq-find
                                 (lambda (hist)
                                   (and
                                    (equal best-1rm
                                           (org-fit--table-1rm (alist-get 'data hist)))
                                    hist))
                                 history))
                 (best-1rm-date (alist-get 'date best-1rm-hist))
                 (best-1rm-row (org-fit--table-1rm-row (alist-get 'data best-1rm-hist))))
            (insert (format "   Best Set Volume: %s\n" (org-fit--value-to-string best-set-volume)))
            (insert (format "   Best Volume: %s\n" (org-fit--value-to-string best-volume)))
            (insert (format "   Best 1RM: %s on %s with %s x%d\n"
                            (org-fit--value-to-string best-1rm)
                            best-1rm-date
                            (org-fit--value-to-string (car best-1rm-row))
                            (cadr best-1rm-row))))
          (special-mode)
          (visual-line-mode 1))
        (display-buffer buf)))))

(defun org-fit-workout-convert-unit (arg)
  "Convert the unit of a value.
With a prefix ARG, convert the value back into the default unit."
  (interactive "p")
  (let* ((line (org-table-current-line))
         (col (org-table-current-column))
         (type (org-fit--column-name-to-type (org-table-get 1 col))))
    (unless (eql type 'weight)
      (user-error "Not on a weight field"))
    (pcase-let* ((`(,val . ,unit) (org-fit--parse-weight
                                   (org-table-get line col)))
                 (new-weight (if (eql unit :kg)
                                 (cons val :lb)
                               (cons val :kg))))
      (if (= arg 4)
          (org-table-put line col (org-fit--weight-to-string (org-fit--convert new-weight
                                                                             org-fit-default-unit)))
        (org-table-put line col (org-fit--weight-to-string new-weight)))
      (org-table-align))))

(defun org-fit-fill-in ()
  "Fill in the current cell according to note."
  (interactive)
  (let* ((note-data (org-fit--extract-table-from-note))
         (cell-value (org-table-get (org-table-current-line) (org-table-current-column)))
         (column-type (org-fit--table-current-column-type))
         (two-input-table-p (equal (org-fit--table-current-column-type 3) 'notes))
         (col1-val (org-table-get (org-table-current-line) 1))
         (col2-val (org-table-get (org-table-current-line) 2)))
    (cond
     ((and (not (alist-get column-type note-data)) two-input-table-p)
      (let* ((other-col (if (= (org-table-current-column) 1) 2 1))
             (other-col-type (org-fit--table-current-column-type other-col))
             (other-col-current-val (org-table-get (org-table-current-line) other-col))
             (other-col-val (org-fit--value-to-string (alist-get other-col-type note-data))))
        (if (and other-col-val (string-blank-p other-col-current-val))
            (org-table-put (org-table-current-line) other-col other-col-val)
          (org-table-put (org-table-current-line) other-col ""))))
     ((not (alist-get column-type note-data))
      (beep))
     ((string-blank-p cell-value)
      (let ((note-val (org-fit--value-to-string (alist-get column-type note-data))))
        (org-table-put (org-table-current-line) (org-table-current-column) note-val))
      (when (not two-input-table-p)
        (org-fit--start-rest-timer)))
     ((not two-input-table-p)
      (org-table-put (org-table-current-line) (org-table-current-column) ""))
     ((string-blank-p col1-val)
      (let ((note-val (org-fit--value-to-string
                       (alist-get (org-fit--table-current-column-type 1) note-data))))
        (org-table-put (org-table-current-line) 1 note-val))
      (org-fit--start-rest-timer))
     ((string-blank-p col2-val)
      (let ((note-val (org-fit--value-to-string
                       (alist-get (org-fit--table-current-column-type 2) note-data))))
        (org-table-put (org-table-current-line) 2 note-val))
      (org-fit--start-rest-timer))
     (t
      (org-table-put (org-table-current-line) 1 "")
      (org-table-put (org-table-current-line) 2 "")))
    (org-table-align)))

(defun org-fit-fill-in-and-next-empty-field ()
  (interactive)
  (let* ((note-data (org-fit--extract-table-from-note))
         (cell-value (org-table-get (org-table-current-line) (org-table-current-column)))
         (column-type (org-fit--table-current-column-type))
         (two-input-table-p (equal (org-fit--table-current-column-type 3) 'notes))
         (prompt (lambda (col-name) (read-string
                                     (format "Cell %s value: " col-name )))))
    (cond
     ((and (not (alist-get column-type note-data)) two-input-table-p)
      (org-table-put (org-table-current-line) (org-table-current-column)
                     (funcall prompt column-type)))
     ((not (alist-get column-type note-data))
      (org-table-put (org-table-current-line) (org-table-current-column)
                     (funcall prompt column-type)))
     ((string-blank-p cell-value)
      (let ((note-val (org-fit--value-to-string (alist-get column-type note-data))))
        (org-table-put (org-table-current-line) (org-table-current-column) note-val))
      (when (not two-input-table-p)
        (org-table-put (org-table-current-line) (org-table-current-column)
                       (funcall prompt column-type)))))
    (org-table-align))
  (org-fit-workout-next-empty-field))

(defun org-fit-edit-and-next-empty-field ()
  (interactive)
  (let* ((column-type (org-fit--table-current-column-type))
         (prompt (lambda (col-name) (read-string
                                     (format "Cell %s value: " col-name )))))
    (org-table-put (org-table-current-line) (org-table-current-column)
                       (funcall prompt column-type))
    (org-table-align))
  (org-fit-workout-next-empty-field))

(defun org-fit--rest-over-p ()
  "Return non-nil if an active rest timer is on and it is over-time."
  (pcase org-fit-workout-rest-time
    (`(,_start . nil)
     (ignore))
    (`(,start ,duration ,_done)
     (> (floor (float-time))
          (+ start duration)))
    (`(,start . ,duration)
     (> (floor (float-time))
          (+ start duration)))
    ('nil
     (ignore))))

(defun org-fit-workout-rest (arg)
  "Start rest if not started.  Otherwise add 15 seconds to rest.
With a prefix ARG, rerun the rest timer at current point from the start.
With two prefix args, prompt the user for a custom time to run."
  (interactive "p")
  (cond
   ((= arg 16)
    (let* ((secs (read-number "Rest seconds: ")))
     (org-fit--start-rest-timer secs)))
   ((= arg 4)
    (let* ((rest-str (org-entry-get (point) org-fit-autorest-property-name)))
     (org-fit--start-rest-timer (when rest-str (org-fit--parse-duration rest-str)))))
   ((org-fit--rest-over-p)
    (org-fit--stop-rest-timer))
   (org-fit-workout-rest-time
    (org-fit--add-rest-time 15 ))
   (t
    (let* ((rest-str (org-entry-get (point) org-fit-autorest-property-name)))
      (org-fit--start-rest-timer (when rest-str (org-fit--parse-duration rest-str)))))))

(defun org-fit-workout-rest-pop ()
  "Use the periosly started rest time, discarding the current."
  (interactive)
  (if org-fit-workout-rest-time-history
      (setq org-fit-workout-rest-time (pop org-fit-workout-rest-time-history))
    (setq org-fit-workout-rest-time nil)))

(defun org-fit-workout-ditto (arg)
  "Copy the value of the row above for the current cell.
If a prefix ARG is provided, copy the entire row."
  (interactive "p")
  (unless (org-fit--on-set-line)
    (user-error "No set on current line"))
  (let* ((current-line (org-table-current-line)))
    (if (<= current-line 2)
        (beep)
      (let ((two-input-table-p (equal (org-fit--table-current-column-type 3) 'notes)))
        (if two-input-table-p
            (let* ((up-val1 (org-table-get (1- current-line) 1))
                   (up-val2 (org-table-get (1- current-line) 2)))
              (if (= arg 4)
                  (progn
                    (org-table-put current-line 1 up-val1 t)
                    (org-table-put current-line 2 up-val2 t))
                (when (string-blank-p (org-table-get current-line 1))
                  (org-table-put current-line 1 up-val1 t))
                (when (string-blank-p (org-table-get current-line 2))
                  (org-table-put current-line 2 up-val2 t))))
          (let* ((up-val (org-table-get (1- current-line) 1)))
            (org-table-put current-line 1 up-val t)))))))

(defun org-fit-set-point (event &optional promote-to-region)
  (interactive "e\np")
  (if (eql (org-element-type (org-element-at-point)) 'headline)
      (progn
        (funcall #'mouse-set-point event promote-to-region)
        (org-fit-workout-right-field))
    (funcall #'mouse-set-point event promote-to-region)))

(defun org-fit--insert-reference-point ()
  "Return a point assocciated with current table.
This is used to know when a user asa switched to editing a
different field."
  (save-excursion
    (while (and (not (bolp))
                (not (looking-at-p "|")))
      (forward-char -1))
    (point)))

(defun org-fit-self-insert-command ()
  "Insert key, erasing table cell when editing for first time."
  (interactive)
  (let ((ref-point (org-fit--insert-reference-point)))
    (when (not (equal org-fit-insert-reference-point ref-point))
      (org-table-blank-field)
      (setq org-fit-insert-reference-point ref-point)
      (setq org-fit-workout-new-data-entered t))
    (call-interactively #'org-self-insert-command)))

(defvar org-fit-exercise-statistics-overlay nil
  "Variable to store exercise statistics overlay.
This overlay is normally placed at the end of an exercie's table.")

(defun org-fit--put-exercise-statistics-overlay ()
  "Put exercise statistics overlay for the current exercise."
  (when (overlayp org-fit-exercise-statistics-overlay)
    (delete-overlay org-fit-exercise-statistics-overlay)
    (setq org-fit-exercise-statistics-overlay nil))
  (when (org-at-table-p)
    (let* ((exercise-name (org-get-heading t t t t))
           (last-exercise (cadr (org-fit--scan-past-workout-exercises exercise-name)))
           (current-data (org-fit-table-to-lisp))
           (volume (org-fit--table-volume current-data))
           (1rm (org-fit--table-1rm current-data))
           (best-set-volume (org-fit--table-best-set-volume current-data))
           (ov (make-overlay (1- (org-table-end)) (1- (org-table-end)))))
      (if last-exercise
          (progn
            (let* ((last-data (alist-get 'data last-exercise))
                   (last-date (alist-get 'date last-exercise))
                   (last-volume (org-fit--table-volume last-data))
                   (last-1rm (org-fit--table-1rm last-data))
                   (last-best-set-volume (org-fit--table-best-set-volume last-data)))
              (overlay-put ov 'after-string
                           (format (concat "\n"
                                           "Best 1RM: %s (%s)\n"
                                           "Total Volume: %s (%s)\n"
                                           "Best set volume: %s (%s)\n"
                                           "                       (%d days ago)\n")
                                   (org-fit--value-to-string 1rm)
                                   (org-fit--value-to-string last-1rm)
                                   (org-fit--value-to-string volume)
                                   (org-fit--value-to-string last-volume)
                                   (org-fit--value-to-string best-set-volume)
                                   (org-fit--value-to-string last-best-set-volume)
                                   (- (org-time-stamp-to-now last-date))))))
        (overlay-put ov 'after-string
                     (format "\nBest 1RM: %s\nTotal Volume: %s\nBest set volume: %s\n"
                             (org-fit--value-to-string 1rm)
                             (org-fit--value-to-string volume)
                             (org-fit--value-to-string best-set-volume))))
      (setq org-fit-exercise-statistics-overlay ov))))

(defun org-fit--header-line ()
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
             (string-match-p (regexp-quote org-fit-active-workout-name)
                             org-clock-heading))
        (setq workout-time (if (> clock-hours 0)
                               (format "%02d:%02d:%02d" clock-hours clock-minutes seconds)
                             (format "%02d:%02d" clock-minutes seconds)))
      (setq workout-time "PAUSED"))
    (when org-fit-workout-rest-time
      (let (rest-start rest-target)
        (pcase org-fit-workout-rest-time
          (`(,start ,target ,_) (setq rest-start start rest-target target))
          (`(,start . ,target) (setq rest-start start rest-target target)))
        (let ((seconds-passed (- (floor (float-time)) rest-start)))
          (if rest-target
              (let* ((bar-width 20)
                     (completed-percentage (/ (float seconds-passed) rest-target))
                     (fill-bar-ct (min (floor (* (mod completed-percentage 1.0) bar-width)) bar-width))
                     (blank-bar-ct (- bar-width fill-bar-ct))
                     (fill-str (make-string fill-bar-ct
                                            (cond
                                             ((>= completed-percentage 9.0)
                                              ?*)
                                             ((>= completed-percentage 1.0)
                                              (min (+ ?0 (1+ (floor completed-percentage))) ?:))
                                             (t ?#))))
                     (blank-str (make-string blank-bar-ct
                                             (cond
                                              ((>= completed-percentage 9.0)
                                               ?#)
                                              ((>= completed-percentage 2.0)
                                               (min (+ ?0 (floor completed-percentage)) ?:))
                                              ((>= completed-percentage 1.0)
                                               (min (+ ?0 (floor completed-percentage)) ?:))
                                              (t ?\s)))))
                (when (> completed-percentage 1.0)
                  (when (= (mod seconds-passed 2) 0)
                    (setq fill-str (propertize fill-str 'face 'error))
                    (setq blank-str (propertize blank-str 'face 'error))))
                (setq rest-time (format "[%s%s] %ds/%ds" fill-str blank-str seconds-passed rest-target)))
            (let* ((mins (/ seconds-passed 60))
                   (secs (mod seconds-passed 60)))
              (if (> mins 0)
                  (setq rest-time (format "REST: %d:%02d" mins secs))
                (setq rest-time (format "REST: %ds" secs))))))))
    (format "%s   %s   %s"
            org-fit-active-workout-name
            workout-time
            rest-time)))

(defun org-fit--rest-over-beep ()
  (when (fboundp #'android-notifications-notify)
    (android-notifications-notify :title "Rest Over" :body "your rest is over" :urgency 'normal
                                  :group "org-fit-rest-over"))
  (dotimes (_ 20)
    (sleep-for 0.01)
    (beep))
  (sleep-for 0.1)
  (dotimes (_ 20)
    (sleep-for 0.01)
    (beep)))

(defun org-fit-workout-update-handler ()
  "Handler function for the timer `org-fit-workout-mode-timer'.
Responsible for updating `header-line-format'."
  (when (and (org-fit--rest-over-p)
             (consp org-fit-workout-rest-time)
             (not (consp (cdr org-fit-workout-rest-time))))
    (run-hooks 'org-fit-rest-timer-over-hook)
    (setq org-fit-workout-rest-time
          (list (car org-fit-workout-rest-time)
                (cdr org-fit-workout-rest-time)
                t)))
  (when org-fit-workout-mode
    (setq-local header-line-format (org-fit--header-line))
    (org-fit--put-exercise-statistics-overlay)))


(define-minor-mode org-fit-workout-mode
  "Toggle `org-fit-workout-mode'.
`org-fit-workout-mode' provides an easy way to input workout data
by adding certain keybindings and automatically tracking certain changes."
  :global nil
  (when (timerp org-fit-workout-mode-timer)
    (cancel-timer org-fit-workout-mode-timer))
  (if (not org-fit-workout-mode)
      (progn
        (setq header-line-format nil)
        (when (overlayp org-fit-exercise-statistics-overlay)
          (delete-overlay org-fit-exercise-statistics-overlay)
          (setq org-fit-exercise-statistics-overlay nil)
          (setq org-fit-workout-rest-time-history nil)
          (setq org-fit-workout-rest-time nil)))
    (setq header-line-format (or org-fit-active-workout-name ""))
    (setq-local org-log-done nil)
    (setq-local org-log-into-drawer nil)
    (setq-local org-todo-log-states nil)
    (setq org-fit-workout-mode-timer (run-at-time nil 0.5 #'org-fit-workout-update-handler))
    (when (fboundp 'set-text-conversion-style)
      (set-text-conversion-style nil))))

(defun org-fit-new-workout (routine-headline)
  "Create a new workout set for selected ROUTINE-HEADLINE."
  (interactive (list (org-fit-read-routine)))
  (let* ((past-routines (org-fit--scan-past-workout-routines routine-headline)))
    (cond
     ((= 0 (length past-routines))
      (org-fit--capture-new-workout (org-fit--get-routine-by-name routine-headline)))
     (t
      (let* ((choice (completing-read "Create a new routine..."
                                      (seq-filter #'identity
                                              `("from original template"
                                                "from most recent exercise routine"
                                                ,(and (> (length past-routines) 1)
                                                      "select from list of past completed routines")))
                                      nil t)))
        (pcase choice
          ("from original template"
           (org-fit--capture-new-workout (org-fit--get-routine-by-name routine-headline)))
          ("from most recent exercise routine"
           (org-fit--capture-new-workout (alist-get 'routine (car past-routines))))
          ("select from list of past completed routines"
           (let* ((dates (seq-map (lambda (x) (alist-get 'date x)) past-routines))
                  (selected-date (completing-read "Use routine completed on date: "
                                                  dates)))
             (org-fit--capture-new-workout
              (alist-get 'routine
                         (seq-find (lambda (r)
                                     (equal selected-date (alist-get 'date r)))
                                   past-routines)))))))))))

(defun org-fit-start-workout ()
  "Start or continue the workout at the current point."
  (interactive)
  (org-fit--set-workout-to-active))

(defun org-fit-new-workout-and-start (routine-headline)
  "Cerate new instance of ROUTINE-HEADLINE and start it."
  (interactive (list (org-fit-read-routine)))
  (org-fit-new-workout routine-headline)
  (org-fit--set-workout-to-active))

(defun org-fit-workout-pause-start-session ()
  "Start/stop the org-clock assocciated with the current workout."
  (interactive)
  (if org-clock-current-task
      (org-clock-out)
    (save-excursion
      (goto-char org-fit-workout-heading-marker)
      (org-clock-in))))

(defun org-fit-workout-quit ()
  "Quit the current workout."
  (interactive)
  (org-fit--process-table-changes)
  (when org-clock-current-task
    (org-clock-out))
  (org-fit-workout-mode 0)
  (widen)
  (save-buffer))

;;; Exercise Display

(defun org-fit-display-exercise (exercise-elt)
  "Display the exercise info buffer for a given org element EXERCISE-ELT."
  (let* ((exercise-name (org-element-property :raw-value exercise-elt))
         (buf (get-buffer-create (format "*org-fit - %s*" exercise-name))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert exercise-name "\n")
        (special-mode)))
    (display-buffer buf)))

;; (defun org-fit-browse-exercises (exercise-elt)
;;   "Display the exercise page for given exercise org element EXERCISE-ELT."
;;   (interactive
;;    (list (org-fit--completing-read-elements (org-fit-exercise-elements))))
;;   (org-fit-display-exercise exercise-elt))

(provide 'org-fit)
;;; org-fit.el ends here
