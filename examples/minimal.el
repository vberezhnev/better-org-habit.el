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
  (setq org-directory "~/Org/agenda/GTD/")
  (setq org-agenda-files '("~/Org/agenda/GTD/org-gtd-tasks.org"))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DONE(d)")))
  (setq org-log-done 'time))

;; Configure org-modern
(use-package org-modern
  :straight t
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  :config
  (setq org-modern-star '("◉" "○" "✸" "✿")
        org-modern-table t
        org-modern-checkbox t
        org-modern-block-fringe t
        org-modern-timestamp t))

;; Configure better-org-habit
(use-package better-org-habit
  ;;:straight (better-org-habit :type git :host github :repo "vberezhnev/better-org-habit.el")
  :load-path "Templates2/Lisp/better-org-habbit/better-org-habit.el"
  :after org
  ;;:init
  ;;(load-file "~/Org/agenda/GTD/better-org-habit-stats.el")
  :config
  (setq hq-categories
        '((:name "PERSONAL" :color "#6A5ACD" :xp 20 :gold 5)
          (:name "EGE" :color "#FF4500" :xp 50 :gold 20)))
  (setq hq-quests
        '((:id 1 :name "Morning Routine"
           :description "Complete morning habits for 5 days"
           :habits ("⏰ - Wake up at 10:00" "💊 - Supplements")
           :required 5 :progress 0 :completed nil
           :reward-xp 200 :reward-gold 100)
          (:id 2 :name "Work Focus"
           :description "Work 3+ hours daily for 7 days"
           :habits ("⚡ - 3+ hours of work")
           :required 7 :progress 0 :completed nil
           :reward-xp 500 :reward-gold 250)))
  (setq hq-market-items
        '((:id "coffee-break" :name "Coffee Break" :cost 20 :category "rest"
           :description "15-minute coffee break" :use-message "Enjoy your coffee!")
          (:id "movie-night" :name "Movie Night" :cost 50 :category "entertainment"
           :description "Watch a movie" :use-message "Enjoy your movie night!")))
  (setq hq-market-categories
        '((:id "rest" :name "Rest" :icon "🌟" :description "Rest and recover")
          (:id "entertainment" :name "Entertainment" :icon "🎮" :description "Fun activities")))
  (setq hq-priority-multipliers
        '((?A . 1.5) (?B . 1.2) (?C . 1.0)))
  (setq org-agenda-custom-commands
        '(("x" "Habits view"
           ((agenda "" ((org-agenda-span 'day)
                        (org-habit-show-habits t)
                        (org-agenda-remove-tags t)
                        (org-agenda-prefix-format "  ∘ %t %s")
                        (org-agenda-files '("~/Org/agenda/GTD/org-gtd-tasks.org"))
                        (org-super-agenda-groups
                         '((:name "Everytime" :tag ("everytime"))
                           (:name "Morning" :tag ("morning"))
                           (:name "Day" :tag ("day"))
                           (:name "Evening" :tag ("evening"))
                           (:discard (:anything))
                           (:discard (:not (:tag "habit")))))))
            (hq-add-quest-info-to-agenda "")))))
  :bind
  (:map org-agenda-mode-map
        ("C-c q" . hq-habits-quest-view)
        ("<f12>" . toggle-org-habit-show-all-today)))

;; Configure org-super-agenda
(use-package org-super-agenda
  :straight t
  :config
  (org-super-agenda-mode 1))
