# org-fit: Workout tracking in Emacs with org-mode

org-fit is a package designed to allow you to easily track your
weightlifting routines as you do them.  It aims to mimic many of the
weightlifting tracking apps on the market today, allowing for quick
and seamless logging of activity.

# Installation

At the moment, org-fit is not published on MELPA so you will need to
install this from source (e.g. add to load path, use-package + ensure,
QUELPA, etc.).

# Setup

The database of exercises, routines, and workouts is all org-mode
files.  For this, you will need the following org-file, which you can customize to your use case:

## Exercises

For the list of known exercises you should have an org-mode file with
headings having the `:ORG_FIT_MUSCLE_GROUP: <biceps/abdominals/etc.>` property set, indicating that it is an exercise.  For example, consider the following file:

```
* Dumbbell Bench Press
  :PROPERTIES:
  :ORG_FIT_MUSCLE_GROUP: chest
  :END:

  1. Lie down on a flat bench with a dumbbell in each hand resting on top of your thighs. The palms of your hands will be facing each other.
  2. Then, using your thighs to help raise the dumbbells up, lift the dumbbells one at a time so that you can hold them in front of you at shoulder width.
  3. Once at shoulder width, rotate your wrists forward so that the palms of your hands are facing away from you. The dumbbells should be just to the sides of your chest, with your upper arm and forearm creating a 90 degree angle. Be sure to maintain full control of the dumbbells at all times. This will be your starting position.
  4. Then, as you breathe out, use your chest to push the dumbbells up. Lock your arms at the top of the lift and squeeze your chest, hold for a second and then begin coming down slowly. Tip: Ideally, lowering the weight should take about twice as long as raising it.
  5. Repeat the movement for the prescribed amount of repetitions of your training program.

* Dumbbell Bicep Curl
  :PROPERTIES:
  :ORG_FIT_MUSCLE_GROUP: biceps
  :END:

  <exercise description here>

* Dumbbell Squat
  :PROPERTIES:
  :ORG_FIT_MUSCLE_GROUP: quadriceps
  :END:

  <exercise description here>
```

## Routines

Routines should be placed in an org-mode file where the routine heading element has the property
`:ORG_FIT_ROUTINE: t` on it.  The template itself should be the exerecises with a table indicating the amount of sets as empty rows.

```
* Full Body Plan
** Full Body Day 1
   :PROPERTIES:
   :ORG_FIT_ROUTINE: t
   :END:

*** Machine Shoulder (Military) Press

 | weight | reps | notes     |
 |--------+------+-----------|
 |        |      | 120kg x13 |
 |        |      |           |
 |        |      |           |

*** Machine Incline Press

 | weight | reps | notes    |
 |--------+------+----------|
 |        |      | 130kg x8 |
 |        |      |          |
 |        |      |          |

*** Dumbbell Lunges

 | weight | reps | notes    |
 |--------+------+----------|
 |        |      | 55kg x10 |
 |        |      |          |

*** Pullups

 | weight | reps | notes |
 |--------+------+-------|
 |        |   12 |       |
 |        |      |       |



** Full Body Day 2
   :PROPERTIES:
   :ORG_FIT_ROUTINE: t
   :END:

*** Reverse Machine Flyes

 | weight | reps | notes    |
 |--------+------+----------|
 |        |      | 60kg x20 |
 |        |      |          |
 |        |      |          |

*** Super ROM Lateral Raise
 | weight | reps | notes    |
 |--------+------+----------|
 |        |      | 35kg x10 |
 |        |      |          |

*** Triceps Overhead Extension with Rope

 | weight | reps | notes      |
 |--------+------+------------|
 |        |      | 31.8kg x13 |
 |        |      |            |
 |        |      |            |

*** Preacher Curl

 | weight | reps | notes    |
 |--------+------+----------|
 |        |      | 50kg x10 |
 |        |      |          |
 |        |      |          |


* Beginner Plans

** Beginner Day 1
   :PROPERTIES:
   :ORG_FIT_ROUTINE: t
   :END:

... and so on ...

```

The names on the exercises should match the names of exercises defined in the previous section.

## Workouts

Lastly, you should have a file to keep track of your workouts.  org-fit will popule this file automatically.  This can be an empty file. Record its name for the Lisp configuration in the next seciton.

## Configuration

You should now have the three files needed: one for exercises, one for routines, and one for workouts.

Set the variable `org-fit-exercise-routine-files` to be a list of your routine and exercise files.

```lisp
(setq org-fit-exercise-routine-files '("/my/path/to/exercises.org" "/my/path/to/routines.org"))
```

Set your workout file to the variable `org-fit-workout-capture-templates` as follows:

```lisp
(setq org-fit-workout-capture-templates `(("Default" (file "/my/path/to/workouts.org"))))
```

# Usage

The following entrypoints exist for org-fit:

- `org-fit-new-workout` Creates a new workout form a selection of template
- `org-fit-start-workout` Given the point is on a workout (presumably created from previous command, which has heading with property `:ORG_FIT_WORKOUT_ROUTINE:`), start org-fit-mode, narrowing to workout
- `org-fit-new-workout-and-start` Combines the previous two commands.

When exercising, you should have tables with the reps, weight, notes columns.  Anything that is in the reps/weight columns is counted as performed.  Notes as to what you *plan* to do should go in the notes section.  This can be used to automatically fill in the reps/weigth columns.  The notes section is divided by semicolon. The first part of the notes is used to fill in the other columns if it has weight rep data, indicated by a weight unit number (e.g. 15kg, 10lb) and a rep count, indicated by `x<NUMBER>` (e.g. x10).  Pressing <kbd>SPC</kbd> will then automatically fill in this data.

```
| reps | weight | notes                  |
|------+--------+------------------------|
|   12 | 15.5kg | x12 15.5kg; 15.5kg x12 |
|   12 |   15.5 | x12 15.5kg             |
|   13 |   15.5 | x13 15.5kg             |
```

With the mode started, the following commands are availablle to you:

- <kbd>up</kbd>, <kbd>down</kbd>, <kbd>left</kbd>, <kbd>right</kbd>, <kbd>p/n/f/b</kbd>: Move the cursor to the previous, next cell, keeping consistency across tables.
- <kbd>tab</kbd>, <kbd>backtab</kbd>: move to the next/previous *empty* cell.
- clicking on headings move cursor to cell and unfolds the exercise
- <kbd>+</kbd>: add set
- <kbd>-</kbd>: remove set
- <kbd>'</kbd>: Add note
- <kbd>W</kbd>: Add warmup set, repeatedly press for more warmups. Configured by `org-fit-warmup-program`
- <kbd>e</kbd>: edit field in separate buffer
- <kbd>&gt;</kbd>: move current workout fields to notes section
- <kbd>t</kbd>: set the type of workout
- <kbd>E</kbd>: add exercise
- <kbd>D</kbd>: remove exercise
- <kbd>S</kbd>: Swap exerices
- <kbd>i</kbd>: Exercise info
- <kbd>u</kbd>: Convert unit
- <kbd>SPC</kbd>: Auto fill-in from notes
- <kbd>"</kbd>: Copy previous set data
- <kbd>volume-down</kbd>: Fill in from notes and go to next empty field (android)
- <kbd>volume-up</kbd>: Prompt user to fill in from notes and go to next empty field (android)
- <kbd>r</kbd>: Rest toggle (start rest -> add 15 secs -> cancel rest if over)
- <kbd>C-c</kbd> <kbd>r</kbd>: Pop to the previous rest timer
- <kbd>P</kbd>: Pause/start session
- <kbd>Q</kbd>: Quit session and clock-out
