;;; better-org-habit-custom.el --- Habit tracking for Org -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2024 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
;; Keywords: outlines, hypermedia, calendar, text
;; URL: https://orgmode.org

;;; Commentary:
;; This file contains the habit tracking code for Org mode.

;;; Code:
(require 'org-macs)
(org-assert-version)
(require 'cl-lib)
(require 'org)
(require 'org-agenda)

(defgroup org-habit nil
  "Options concerning habit tracking in Org mode."
  :tag "Org Habit"
  :group 'org-progress)

(defcustom org-habit-graph-column 40
  "Column for habit consistency graphs."
  :group 'org-habit
  :type 'integer)

(defcustom org-habit-preceding-days 21
  "Days before today in consistency graphs."
  :group 'org-habit
  :type 'integer)

(defcustom org-habit-following-days 7
  "Days after today in consistency graphs."
  :group 'org-habit
  :type 'integer)

(defcustom org-habit-show-habits t
  "If non-nil, show habits in agenda buffers."
  :group 'org-habit
  :type 'boolean)

(defcustom org-habit-show-habits-only-for-today t
  "If non-nil, show habits only on today's agenda."
  :group 'org-habit
  :type 'boolean)

(defcustom org-habit-show-all-today nil
  "If non-nil, show all habits on today's agenda."
  :group 'org-habit
  :type 'boolean)

(defcustom org-habit-today-glyph ?!
  "Glyph for today."
  :group 'org-habit
  :type 'character)

(defcustom org-habit-completed-glyph ?*
  "Glyph for completed days."
  :group 'org-habit
  :type 'character)

(defcustom org-habit-alert-glyph ?‼
  "Glyph for alert days."
  :group 'org-habit
  :type 'character)

(defcustom org-habit-overdue-glyph ?○
  "Glyph for overdue days."
  :group 'org-habit
  :type 'character)

(defcustom org-habit-clear-glyph ?○
  "Glyph for clear days."
  :group 'org-habit
  :type 'character)

(defcustom org-habit-ready-future-glyph ?○
  "Glyph for ready future days."
  :group 'org-habit
  :type 'character)

(defcustom org-habit-today-glyph ?◎
  "Glyph for today."
  :group 'org-habit
  :type 'character)

(defcustom org-habit-completed-glyph ?●
  "Glyph for completed days."
  :group 'org-habit
  :type 'character)

(defcustom org-habit-alert-glyph ?○
  "Glyph for alert days."
  :group 'org-habit
  :type 'character)

(defcustom org-habit-show-done-always-green t
  "If non-nil, show completed habits in green."
  :group 'org-habit
  :type 'boolean)

(defcustom org-habit-scheduled-past-days nil
  "Value for habits instead of `org-scheduled-past-days'."
  :group 'org-habit
  :type '(choice integer (const nil))
  :package-version '(Org . "9.3"))

(defface org-habit-clear-face
  '((((background light)) (:background "#8270f9"))
    (((background dark)) (:background "blue")))
  "Face for days when a task is not yet due."
  :group 'org-habit
  :group 'org-faces)

(defface org-habit-clear-future-face
  '((((background light)) (:background "#d6e4fc"))
    (((background dark)) (:background "midnightblue")))
  "Face for future days when a task is not due."
  :group 'org-habit
  :group 'org-faces)

(defface org-habit-ready-face
  '((((background light)) (:background "#4df946"))
    (((background dark)) (:background "forestgreen")))
  "Face for days when a task should start."
  :group 'org-habit
  :group 'org-faces)

(defface org-habit-ready-future-face
  '((((background light)) (:background "#acfca9"))
    (((background dark)) (:background "darkgreen")))
  "Face for future days when a task should start."
  :group 'org-habit
  :group 'org-faces)

(defface org-habit-alert-face
  '((((background light)) (:background "#f5f946"))
    (((background dark)) (:background "gold")))
  "Face for days when a task is due."
  :group 'org-habit
  :group 'org-faces)

(defface org-habit-alert-future-face
  '((((background light)) (:background "#fafca9"))
    (((background dark)) (:background "darkgoldenrod")))
  "Face for future days when a task is due."
  :group 'org-habit
  :group 'org-faces)

(defface org-habit-overdue-face
  '((((background light)) (:background "#f9372d"))
    (((background dark)) (:background "firebrick")))
  "Face for overdue days."
  :group 'org-habit
  :group 'org-faces)

(defface org-habit-overdue-future-face
  '((((background light)) (:background "#fc9590"))
    (((background dark)) (:background "darkred")))
  "Face for future overdue days."
  :group 'org-habit
  :group 'org-faces)

(defun org-habit-duration-to-days (ts)
  "Convert duration string to days."
  (if (string-match "\\([0-9]+\\)\\([dwmy]\\)" ts)
      (floor (* (string-to-number (match-string 1 ts))
                (cdr (assoc (match-string 2 ts)
                            '(("d" . 1) ("w" . 7) ("m" . 30.4) ("y" . 365.25))))))
    (error "Invalid duration string: %s" ts)))

(defun org-is-habit-p (&optional epom)
  "Is the task at EPOM or point a habit?"
  (string= "habit" (org-entry-get epom "STYLE" 'selective)))

(defun org-habit-parse-todo (&optional pom)
  "Parse TODO for habit data."
  (save-excursion
    (if pom (goto-char pom))
    (cl-assert (org-is-habit-p (point)))
    (let* ((scheduled (org-get-scheduled-time (point)))
           (scheduled-repeat (org-get-repeat (org-entry-get (point) "SCHEDULED")))
           (end (org-entry-end-position))
           (habit-entry (org-no-properties (nth 4 (org-heading-components))))
           closed-dates deadline dr-days sr-days sr-type)
      (if scheduled
          (setq scheduled (time-to-days scheduled))
        (error "Habit %s has no scheduled date" habit-entry))
      (unless scheduled-repeat
        (error "Habit `%s' has no scheduled repeat period" habit-entry))
      (setq sr-days (org-habit-duration-to-days scheduled-repeat)
            sr-type (progn (string-match "[\\.+]?\\+" scheduled-repeat)
                           (match-string-no-properties 0 scheduled-repeat)))
      (unless (> sr-days 0)
        (error "Habit %s scheduled repeat period is less than 1d" habit-entry))
      (when (string-match "/\\([0-9]+[dwmy]\\)" scheduled-repeat)
        (setq dr-days (org-habit-duration-to-days
                       (match-string-no-properties 1 scheduled-repeat)))
        (if (<= dr-days sr-days)
            (error "Habit %s deadline repeat period is less than or equal to scheduled (%s)"
                   habit-entry scheduled-repeat))
        (setq deadline (+ scheduled (- dr-days sr-days))))
      (org-back-to-heading t)
      (let* ((maxdays (+ org-habit-preceding-days org-habit-following-days))
             (reversed org-log-states-order-reversed)
             (search (if reversed 're-search-forward 're-search-backward))
             (limit (if reversed end (point)))
             (count 0)
             (re (format
                  "^[ \t]*-[ \t]+\\(?:State \"%s\".*%s%s\\)"
                  (regexp-opt org-done-keywords)
                  org-ts-regexp-inactive
                  (let ((value (cdr (assq 'done org-log-note-headings))))
                    (if (not value) ""
                      (concat "\\|"
                              (org-replace-escapes
                               (regexp-quote value)
                               `(("%d" . ,org-ts-regexp-inactive)
                                 ("%D" . ,org-ts-regexp)
                                 ("%s" . "\"\\S-+\"")
                                 ("%S" . "\"\\S-+\"")
                                 ("%t" . ,org-ts-regexp-inactive)
                                 ("%T" . ,org-ts-regexp)
                                 ("%u" . ".*?")
                                 ("%U" . ".*?")))))))))
        (unless reversed (goto-char end))
        (while (and (< count maxdays) (funcall search re limit t))
          (push (time-to-days
                 (org-time-string-to-time
                  (or (match-string-no-properties 1)
                      (match-string-no-properties 2))))
                closed-dates)
          (setq count (1+ count))))
      (list scheduled sr-days deadline dr-days closed-dates sr-type))))

(defsubst org-habit-scheduled (habit)
  (nth 0 habit))
(defsubst org-habit-scheduled-repeat (habit)
  (nth 1 habit))
(defsubst org-habit-deadline (habit)
  (let ((deadline (nth 2 habit)))
    (or deadline
        (if (nth 3 habit)
            (+ (org-habit-scheduled habit)
               (1- (org-habit-scheduled-repeat habit)))
          (org-habit-scheduled habit)))))
(defsubst org-habit-deadline-repeat (habit)
  (or (nth 3 habit)
      (org-habit-scheduled-repeat habit)))
(defsubst org-habit-done-dates (habit)
  (nth 4 habit))
(defsubst org-habit-repeat-type (habit)
  (nth 5 habit))

(defun org-habit-get-urgency (habit &optional moment)
  "Determine the relative urgency of a habit."
  (let ((pri 1000)
        (now (if moment (time-to-days moment) (org-today)))
        (scheduled (org-habit-scheduled habit))
        (deadline (org-habit-deadline habit)))
    (setq pri (+ pri (* (- now scheduled) 10)))
    (if (and (/= scheduled deadline)
             (= now deadline))
        (setq pri (+ pri 50)))
    (let ((slip (- now (1- deadline))))
      (if (> slip 0)
          (setq pri (+ pri (* slip 100)))
        (setq pri (+ pri (* slip 10)))))
    pri))

(defun org-habit-get-faces (habit &optional now-days scheduled-days donep)
  "Return faces for HABIT relative to NOW-DAYS and SCHEDULED-DAYS."
  (let* ((scheduled (or scheduled-days (org-habit-scheduled habit)))
         (s-repeat (org-habit-scheduled-repeat habit))
         (d-repeat (org-habit-deadline-repeat habit))
         (deadline (if scheduled-days
                       (+ scheduled-days (- d-repeat s-repeat))
                     (org-habit-deadline habit)))
         (m-days (or now-days (time-to-days nil))))
    (cond
     ((< m-days scheduled)
      '(org-habit-clear-face . org-habit-clear-future-face))
     ((< m-days deadline)
      '(org-habit-ready-face . org-habit-ready-future-face))
     ((= m-days deadline)
      (if donep
          '(org-habit-ready-face . org-habit-ready-future-face)
        '(org-habit-alert-face . org-habit-alert-future-face)))
     ((and org-habit-show-done-always-green donep)
      '(org-habit-ready-face . org-habit-ready-future-face))
     (t '(org-habit-overdue-face . org-habit-overdue-future-face)))))

(defun org-habit-build-graph (habit starting current ending)
  "Build a graph for HABIT from STARTING to ENDING."
  (let* ((all-done-dates (sort (org-habit-done-dates habit) #'<))
         (done-dates all-done-dates)
         (scheduled (org-habit-scheduled habit))
         (s-repeat (org-habit-scheduled-repeat habit))
         (start (time-to-days starting))
         (now (time-to-days current))
         (end (time-to-days ending))
         (graph (make-string (1+ (- end start)) ?\s))
         (index 0)
         last-done-date)
    (while (and done-dates (< (car done-dates) start))
      (setq last-done-date (car done-dates)
            done-dates (cdr done-dates)))
    (while (< start end)
      (let* ((in-the-past-p (< start now))
             (todayp (= start now))
             (donep (and done-dates (= start (car done-dates))))
             (faces
              (if (and in-the-past-p
                       (not last-done-date)
                       (not (< scheduled now)))
                  (if (and all-done-dates (= (car all-done-dates) start))
                      '(org-habit-ready-face . org-habit-ready-future-face)
                    '(org-habit-clear-face . org-habit-clear-future-face))
                (org-habit-get-faces
                 habit start
                 (and in-the-past-p
                      last-done-date
                      (let ((type (org-habit-repeat-type habit)))
                        (cond
                         ((null done-dates) scheduled)
                         ((equal type ".+") (+ last-done-date s-repeat))
                         ((equal type "+")
                          (- scheduled (* (length done-dates) s-repeat)))
                         (t
                          (let* ((first-done (car all-done-dates))
                                 (s (let ((shift (mod (- scheduled first-done)
                                                      s-repeat)))
                                      (+ (if (= shift 0) s-repeat shift)
                                         first-done))))
                            (if (= first-done last-done-date) s
                              (catch :exit
                                (dolist (done (cdr all-done-dates) s)
                                  (cl-incf s (* (1+ (/ (max (- done s) 0)
                                                       s-repeat))
                                                s-repeat))
                                  (when (= done last-done-date)
                                    (throw :exit s))))))))))
                 donep)))
             markedp face)
        (cond
         (donep
          (aset graph index org-habit-completed-glyph)
          (setq markedp t)
          (while (and done-dates (= start (car done-dates)))
            (setq last-done-date (car done-dates)
                  done-dates (cdr done-dates))))
         (todayp
          (aset graph index org-habit-today-glyph))
         ((eq (if in-the-past-p (car faces) (cdr faces)) 'org-habit-ready-future-face)
          (aset graph index org-habit-ready-future-glyph))
         ((eq (if in-the-past-p (car faces) (cdr faces)) 'org-habit-clear-face)
          (aset graph index org-habit-clear-glyph))
         ((eq (if in-the-past-p (car faces) (cdr faces)) 'org-habit-alert-face)
          (aset graph index org-habit-alert-glyph))
         ((eq (if in-the-past-p (car faces) (cdr faces)) 'org-habit-overdue-face)
          (aset graph index org-habit-overdue-glyph))
         (t
          (aset graph index org-habit-ready-future-glyph)))
        (setq face (if (or in-the-past-p todayp)
                       (car faces)
                     (cdr faces)))
        (when (and in-the-past-p
                   (not (eq face 'org-habit-overdue-face))
                   (not markedp))
          (setq face (cdr faces)))
        (put-text-property index (1+ index) 'face face graph)
        (put-text-property index (1+ index)
                           'help-echo
                           (concat (format-time-string
                                    (org-time-stamp-format)
                                    (time-add starting (days-to-time (- start (time-to-days starting)))))
                                   (if donep " DONE" ""))
                           graph))
      (setq start (1+ start)
            index (1+ index)))
    graph))

(defun org-habit-insert-consistency-graphs (&optional line)
  "Insert consistency graph for habitual tasks."
  (let ((inhibit-read-only t)
        (buffer-invisibility-spec '(org-link))
        (moment (time-subtract nil (* 3600 org-extend-today-until))))
    (save-excursion
      (goto-char (if line (line-beginning-position) (point-min)))
      (while (not (eobp))
        (let ((habit (get-text-property (point) 'org-habit-p))
              (invisible-prop (get-text-property (point) 'invisible)))
          (when habit
            (move-to-column org-habit-graph-column t)
            (delete-char (min (+ 1 org-habit-preceding-days
                                 org-habit-following-days)
                              (- (line-end-position) (point))))
            (insert-before-markers
             (org-habit-build-graph
              habit
              (time-subtract moment (days-to-time org-habit-preceding-days))
              moment
              (time-add moment (days-to-time org-habit-following-days))))
            (when invisible-prop
              (put-text-property
               (- (point) org-habit-graph-column) (point)
               'invisible invisible-prop))))
        (forward-line)))))

(defun org-habit-toggle-habits ()
  "Toggle display of habits in an agenda buffer."
  (interactive)
  (org-agenda-check-type t 'agenda)
  (setq org-habit-show-habits (not org-habit-show-habits))
  (org-agenda-redo)
  (org-agenda-set-mode-name)
  (message "Habits turned %s" (if org-habit-show-habits "on" "off")))

(defun org-habit-toggle-display-in-agenda (arg)
  "Toggle display of habits in agenda."
  (interactive "P")
  (if (not arg)
      (org-habit-toggle-habits)
    (org-agenda-check-type t 'agenda)
    (setq org-habit-show-all-today (not org-habit-show-all-today))
    (when org-habit-show-habits (org-agenda-redo))))

(org-defkey org-agenda-mode-map "K" 'org-habit-toggle-display-in-agenda)

(provide 'better-org-habit-custom)
;;; better-org-habit-custom.el ends here
