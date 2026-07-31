;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install org early to prevent version mismatch
(straight-use-package 'org)

;; Install use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Configure Org mode
(use-package org
  :config
  (setq org-directory "~/Org/")
  (setq org-agenda-files '("~/Org/habits.org"))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DONE(d)")))
  (setq org-log-done 'time))

;; Configure better-org-habit
(use-package better-org-habit
  ;; :straight (better-org-habit :type git :host github :repo "vberezhnev/better-org-habit.el")
  :load-path "~/Templates/Lisp/better-org-habbit/"
  :demand t
  :init
  (setq better-org-habit-view-files '("~/Org/habits.org")
        better-org-habit-view-groups
        '((:name "Everytime" :tag ("everytime"))
          (:name "Morning" :tag ("morning"))
          (:name "Day" :tag ("day"))
          (:name "Evening" :tag ("evening"))
          (:discard (:anything))
          (:discard (:not (:tag "habit"))))))

;; Optional: grouped habit view with org-super-agenda
(use-package org-super-agenda
  :straight t
  :config
  (org-super-agenda-mode 1))
