;;; better-org-habit.el --- Enhanced habit tracking for Org mode -*- lexical-binding: t; -*-

;; Author: Vladimir Berezhnev
;; Keywords: org, habits
;; Version: 0.2
;; URL: https://github.com/vberezhnev/better-org-habit.el

;;; Commentary:
;; This package enhances Org mode habit tracking with configurable
;; faces and glyphs, per-habit streak counting in the agenda, and
;; integration with `better-org-habit-stats'.

;;; Code:
;; The habit graph cells render as squares only with a proportional (non-Mono)
;; font. The org-habit-* faces below force :family "Iosevka Nerd Font", which
;; makes the ○/● glyphs ~square; a Mono fallback produces narrow rectangles.

;; (require 'better-org-habit-custom)
;; (require 'better-org-habit-stats)

(require 'org)
(require 'org-agenda)
(require 'cl-lib)

(progn
  (custom-set-faces
   '(org-habit-clear-face
     ((t (:background "pale green"
                     :foreground "white"
                     :width expanded
                     :height 1.0
                     :family "Iosevka Nerd Font"
                     :box (:line-width (1 . 1) :color "white")))))
   '(org-habit-clear-future-face
     ((t (:background "gray"
                     :foreground "white"
                     :width expanded
                     :height 1.0
                     :family "Iosevka Nerd Font"
                     :box (:line-width (1 . 1) :color "white")))))
   '(org-habit-alert-future-face
     ((t (:background "light coral"
                     :foreground "white"
                     :width expanded
                     :height 1.0
                     :family "Iosevka Nerd Font"
                     :box (:line-width (1 . 1) :color "white")))))
   '(org-habit-alert-face
     ((t (:background "light coral"
                     :foreground "white"
                     :width expanded
                     :height 1.0
                     :family "Iosevka Nerd Font"
                     :box (:line-width (1 . 1) :color "white")))))
   '(org-habit-overdue-face
     ((t (:background "light coral"
                     :foreground "white"
                     :width expanded
                     :height 1.0
                     :family "Iosevka Nerd Font"
                     :box (:line-width (1 . 1) :color "white")))))
   '(org-habit-overdue-future-face
     ((t (:background "gray"
                     :foreground "white"
                     :width expanded
                     :height 1.0
                     :family "Iosevka Nerd Font"
                     :box (:line-width (1 . 1) :color "white")))))
   '(org-habit-ready-face
     ((t (:background "pale green"
                     :foreground "white"
                     :width expanded
                     :height 1.0
                     :family "Iosevka Nerd Font"
                     :box (:line-width (1 . 1) :color "white")))))
   '(org-habit-ready-future-face
     ((t (:background "gray"
                     :foreground "white"
                     :width expanded
                     :height 1.0
                     :family "Iosevka Nerd Font"
                     :box (:line-width (1 . 1) :color "white"))))))
  (setq org-habit-following-days 1
        org-habit-preceding-days 14 ;;6
        org-habit-show-habits nil
        org-habit-show-all-today t
        org-habit-graph-column 45 ;;60
        org-habit-overdue-glyph ?○
        org-habit-alert-glyph ?○
        org-habit-ready-future-glyph ?○
        org-habit-today-glyph ?◎
        org-habit-completed-glyph ?●
        org-habit-show-done-always-green t))

(defun toggle-org-habit-show-all-today ()
  "Toggle the value of `org-habit-show-all-today' between t and nil."
  (interactive)
  (setq org-habit-show-all-today (not org-habit-show-all-today))
  (message "org-habit-show-all-today is now %s"
           (if org-habit-show-all-today "t" "nil"))
  (org-agenda-refresh))
(define-key org-agenda-mode-map (kbd "<f12>") 'toggle-org-habit-show-all-today)

;; Org-habit-stats: Load for habit statistics
(add-hook 'org-after-todo-state-change-hook 'org-habit-stats-update-properties)
(add-hook 'org-agenda-mode-hook
          (lambda () (define-key org-agenda-mode-map "Z" 'org-habit-stats-view-next-habit-in-agenda)))

(defun hq--map-agenda-habit-lines (fn)
  "Call FN at each habit line in the *Org Agenda* buffer.
FN is called with point at the beginning of the habit line."
  (when (get-buffer "*Org Agenda*")
    (with-current-buffer "*Org Agenda*"
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when (get-text-property (point) 'org-habit-p)
            (funcall fn))
          (forward-line 1))))))

(defun hq--habit-name-at-marker (marker)
  "Return the heading text at MARKER, or nil."
  (when marker
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char (marker-position marker))
        (org-get-heading t t t t)))))

(defun hq--habit-streak (habit)
  "Return the current streak of consecutive done days for HABIT.
Counts backwards from today (or yesterday if today is not done yet)."
  (let* ((done-dates (org-habit-done-dates habit))
         (today (org-today))
         (day (if (memq today done-dates) today (1- today)))
         (streak 0))
    (while (memq day done-dates)
      (setq streak (1+ streak)
            day (1- day)))
    streak))

;;; Custom Functions
(defun org-agenda-refresh ()
  "Refresh all `org-agenda' buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-agenda-mode)
        (org-agenda-maybe-redo)))))

(defun org-habit-streak-count ()
  "Display current streak for each habit in org-agenda."
  (hq--map-agenda-habit-lines
   (lambda ()
     (let* ((habit (get-text-property (point) 'org-habit-p))
            (streak (if habit (hq--habit-streak habit) 0)))
       (end-of-line)
       (insert (format " [🔥 %d]" streak))))))

;; Hooks and Keybindings
(add-hook 'org-agenda-finalize-hook 'org-habit-streak-count)

(provide 'better-org-habit)
;;; better-org-habit.el ends here

