;;; better-org-habit.el --- Enhanced habit tracking with gamification for Org mode -*- lexical-binding: t; -*-

;; Author: Vladimir Berezhnev
;; Keywords: org, habits, gamification
;; Version: 0.2
;; URL: https://github.com/vberezhnev/better-org-habit.el

;;; Commentary:
;; This package enhances Org mode with a gamified habit tracking system,
;; including quests, a market, and a customizable UI for tracking habits and tasks.

;;; Code:
;; FIXME: org-habit-faces display correctly (as squares) only with specific fonts (e.g., Aporetic).
;; With Emacs's default font, org-habit-faces appear as narrow rectangles.
;; Consider how to fix this without requiring a specific font to be set globally in Emacs.

(require 'better-org-habit-custom)
(require 'better-org-habit-stats)

(require 'org)
(require 'org-agenda)
(require 'cl-lib)
(require 'widget)
(require 'wid-edit)

;;; Customization Group
(defgroup habit-quest-system nil
  "Customization group for Habit Quest System."
  :group 'org
  :prefix "hq-")

;;; Market Categories
(defcustom hq-market-categories
  '((:id "rest" :name "Rest" :icon "🌟" :description "Ways to rest and recover")
    (:id "entertainment" :name "Entertainment" :icon "🎮" :description "Fun activities")
    (:id "bonus" :name "Bonuses" :icon "✨" :description "Special privileges")
    (:id "rare" :name "Rare Items" :icon "💎" :description "Unique items"))
  "List of item categories in the Habit Quest System store.
Each category must be a plist with fields :id, :name, :icon, and :description."
  :type '(repeat
          (plist
           :key-type symbol
           :value-type (choice string number boolean)))
  :group 'habit-quest-system)

;;; Market Items
(defcustom hq-market-items
  '((:id "break-30" :name "30-Minute Break" :cost 30 :category "rest"
     :description "A short break to rest"
     :use-message "Relax and recharge!")
    (:id "break-60" :name "One-Hour Rest" :cost 50 :category "rest"
     :description "A full hour of rest"
     :use-message "Enjoy a full hour of relaxation!")
    (:id "nap" :name "Power Nap" :cost 80 :category "rest"
     :description "15-20 minutes of sleep to restore energy"
     :use-message "Enjoy a refreshing nap!")
    (:id "episode" :name "TV Show Episode" :cost 60 :category "entertainment"
     :description "Watch one episode of your favorite show"
     :use-message "Enjoy watching!")
    (:id "movie" :name "Movie" :cost 100 :category "entertainment"
     :description "Watch a full movie"
     :use-message "Enjoy the movie!")
    (:id "gaming" :name "Gaming Session" :cost 120 :category "entertainment"
     :description "1 hour of your favorite games"
     :use-message "Have fun gaming!")
    (:id "youtube" :name "YouTube Time" :cost 40 :category "entertainment"
     :description "30 minutes on YouTube"
     :use-message "Enjoy watching!")
    (:id "delay-1h" :name "One-Hour Task Delay" :cost 70 :category "bonus"
     :description "Postpone a task by 1 hour"
     :use-message "Task delayed by one hour.")
    (:id "music" :name "Music During Work" :cost 30 :category "bonus"
     :description "1 hour of music while working"
     :use-message "Enjoy the music!")
    (:id "late-wake" :name "Late Wake-Up" :cost 150 :category "bonus"
     :description "Permission to wake up one hour later"
     :use-message "Sleep in a bit longer!")
    (:id "day-off" :name "Day Off" :cost 500 :category "rare"
     :description "A full day off from all tasks"
     :discountable nil
     :use-message "Enjoy your well-deserved day off!")
    (:id "weekend" :name "Gaming Weekend" :cost 1000 :category "rare"
     :description "A full weekend dedicated to gaming"
     :discountable nil
     :use-message "Have a fun gaming weekend!"))
  "List of items in the Habit Quest System store.
Each item must be a plist with fields :id, :name, :cost, :category, :description,
:use-message, and optional :discountable."
  :type '(repeat
          (plist
           :key-type symbol
           :value-type (choice string number boolean)))
  :group 'habit-quest-system)

;;; Quests
(defcustom hq-quests
  '((:id 1 :name "Path to Mindfulness"
     :description "Complete all three meditations for 5 consecutive days"
     :habits ("🎯 - Morning Meditation" "🌟 - Midday Meditation" "🌿 - Evening Meditation")
     :required 5 :progress 0 :completed nil
     :reward-xp 200 :reward-gold 100)
    (:id 2 :name "Iron Discipline"
     :description "Wake up at 05:30 for 7 consecutive days"
     :habits ("⏰ - Wake up at 05:30")
     :required 7 :progress 0 :completed nil
     :reward-xp 300 :reward-gold 150)
    (:id 3 :name "Energy Balance"
     :description "Drink 2 liters of water and walk 10k steps for 10 consecutive days"
     :habits ("💧 - 2 liters of water" "🚶 - 10k steps")
     :required 10 :progress 0 :completed nil
     :reward-xp 400 :reward-gold 200)
    (:id 4 :name "Study Focus"
     :description "Prepare for exams for 5 consecutive days"
     :habits ("📝 - Exam Preparation")
     :required 5 :progress 0 :completed nil
     :reward-xp 400 :reward-gold 400)
    (:id 5 :name "Vigor Mode"
     :description "Take a contrast shower for 7 consecutive days"
     :habits ("🚿 - Contrast Shower")
     :required 7 :progress 0 :completed nil
     :reward-xp 140 :reward-gold 70))
  "List of quests in the Habit Quest System.
Each quest must be a plist with fields :id, :name, :description, :habits, :required,
:progress, :completed, :reward-xp, and :reward-gold."
  :type '(repeat
          (plist
           :key-type symbol
           :value-type (choice string number boolean (repeat string))))
  :group 'habit-quest-system)

(defcustom hq-categories
  '((:name "CORE" :color "#FFD700" :xp 40 :gold 15)
    (:name "ASCENT" :color "#4CAF50" :xp 30 :gold 10)
    (:name "PERSONAL" :color "#6A5ACD" :xp 20 :gold 5))
  "List of task categories for the Habit Quest System.
Each category must be a plist with fields :name (string, category name),
:color (string, color in #RRGGBB format), :xp (number, base experience),
:gold (number, base gold)."
  :type '(repeat
          (plist
           :key-type symbol
           :value-type (choice string number)))
  :group 'habit-quest-system)

(progn
  (custom-set-faces
   '(org-habit-clear-face
     ((t (:background "pale green"
                     :foreground "white"
                     :width expanded
                     :height 1.0
                     :family "Iosevka"
                     :box (:line-width (1 . 1) :color "white")))))
   '(org-habit-clear-future-face
     ((t (:background "gray"
                     :foreground "white"
                     :width expanded
                     :height 1.0
                     :family "Iosevka"
                     :box (:line-width (1 . 1) :color "white")))))
   '(org-habit-alert-future-face
     ((t (:background "light coral"
                     :foreground "white"
                     :width expanded
                     :height 1.0
                     :family "Iosevka"
                     :box (:line-width (1 . 1) :color "white")))))
   '(org-habit-alert-face
     ((t (:background "light coral"
                     :foreground "white"
                     :width expanded
                     :height 1.0
                     :family "Iosevka"
                     :box (:line-width (1 . 1) :color "white")))))
   '(org-habit-overdue-face
     ((t (:background "light coral"
                     :foreground "white"
                     :width expanded
                     :height 1.0
                     :family "Iosevka"
                     :box (:line-width (1 . 1) :color "white")))))
   '(org-habit-overdue-future-face
     ((t (:background "gray"
                     :foreground "white"
                     :width expanded
                     :height 1.0
                     :family "Iosevka"
                     :box (:line-width (1 . 1) :color "white")))))
   '(org-habit-ready-face
     ((t (:background "pale green"
                     :foreground "white"
                     :width expanded
                     :height 1.0
                     :family "Iosevka"
                     :box (:line-width (1 . 1) :color "white")))))
   '(org-habit-ready-future-face
     ((t (:background "gray"
                     :foreground "white"
                     :width expanded
                     :height 1.0
                     :family "Iosevka"
                     :box (:line-width (1 . 1) :color "white"))))))
  (setq org-habit-following-days 1
        org-habit-preceding-days 6
        org-habit-show-habits nil
        org-habit-show-all-today t
        org-habit-graph-column 60
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

;;; Quest System Core
(defvar hq-xp 0 "Total character experience")
(defvar hq-level 1 "Character level")
(defvar hq-gold 0 "Character gold")
(defvar hq-inventory '() "Items in the player's inventory.")

(defun hq-save-data ()
  "Save the quest system data."
  (with-temp-file "~/.emacs.d/habit-quest-data.el"
    (prin1 (list hq-xp
                 hq-level
                 hq-gold
                 hq-quests
                 hq-inventory
                 hq-daily-bonus
                 hq-last-bonus-date)
           (current-buffer))))

(defun hq-load-data ()
  "Load the quest system data."
  (when (file-exists-p "~/.emacs.d/habit-quest-data.el")
    (with-temp-buffer
      (insert-file-contents "~/.emacs.d/habit-quest-data.el")
      (goto-char (point-min))
      (let ((data (read (current-buffer))))
        (setq hq-xp (nth 0 data)
              hq-level (nth 1 data)
              hq-gold (nth 2 data)
              hq-quests (nth 3 data)
              hq-inventory (nth 4 data)
              hq-daily-bonus (nth 5 data)
              hq-last-bonus-date (nth 6 data)
              hq-penalty-history (nth 7 data))))))

(defun hq-add-xp-and-gold (xp gold)
  "Add experience and gold, updating the level."
  (setq hq-xp (+ hq-xp xp))
  (setq hq-level (1+ (/ hq-xp 100)))
  (setq hq-gold (+ hq-gold gold))
  (hq-save-data)
  (message "🏆 Gained: +%d XP, +%d gold" xp gold))

(defun hq-ui-width ()
  "Get the working width for the UI."
  (min 70 (- (window-width) 4)))

(defun hq-make-divider (&optional char)
  "Create a simple divider."
  (let ((divider-char (or char ?-)))
    (propertize (make-string (hq-ui-width) divider-char)
                'face '(:foreground "#4A90E2"))))

(defun hq-make-header (title)
  "Create a centered header."
  (let* ((width (hq-ui-width))
         (title-len (length title))
         (padding-left (/ (- width title-len) 2))
         (padding-right (- width title-len padding-left)))
    (concat
     (propertize (make-string padding-left ?\s) 'face '(:foreground "#4A90E2"))
     (propertize title 'face '(:foreground "#FFD700" :weight bold))
     (propertize (make-string padding-right ?\s) 'face '(:foreground "#4A90E2")))))

(defun hq-make-content (content)
  "Create a content string with fixed width."
  (let* ((width (hq-ui-width))
         (content-len (length content))
         (padding (max 0 (- width content-len))))
    (concat content (make-string padding ?\s))))

(defun hq-progress-bar (current max width)
  "Create a simple ASCII progress bar."
  (let* ((current-val (or current 0))
         (max-val (or max 1))
         (ratio (if (> max-val 0) (/ (float current-val) max-val) 0))
         (filled (round (* width ratio)))
         (empty (- width filled)))
    (concat "["
            (propertize (make-string filled ?#) 'face '(:foreground "#4CAF50"))
            (propertize (make-string empty ?.) 'face '(:foreground "#666666"))
            "]")))

(defun hq-show-potential-reward ()
  "Show the potential reward for completing a task."
  (interactive)
  (let* ((category (or (org-entry-get nil "CATEGORY") "PERSONAL"))
         (category-data (seq-find (lambda (cat)
                                    (string= (plist-get cat :name) category))
                                  hq-categories))
         (base-xp (or (plist-get category-data :xp) 20))
         (base-gold (or (plist-get category-data :gold) 5))
         (category-color (or (plist-get category-data :color) "#6A5ACD"))
         (priority (org-entry-get nil "PRIORITY"))
         (priority-mult (or (cdr (assoc (and priority (aref priority 0))
                                        hq-priority-multipliers))
                            1.0))
         (clock-minutes (org-clock-sum-current-item))
         (time-bonus (hq-calculate-time-bonus clock-minutes))
         (deadline-bonus (hq-calculate-deadline-bonus))
         (potential-xp (round (* base-xp priority-mult time-bonus deadline-bonus)))
         (potential-gold (round (* base-gold priority-mult time-bonus deadline-bonus)))
         (xp-to-next-level (- 100 (mod hq-xp 100))))
    (goto-char (point-max))
    (insert "\n")
    (insert (hq-make-divider ?=) "\n")
    (insert (hq-make-header "✨ QUEST SYSTEM ✨") "\n")
    (insert (hq-make-divider ?=) "\n")
    (insert (propertize " CHARACTER " 'face '(:foreground "#5C85D6" :weight bold)) "\n")
    (insert (format " Level: %d   XP: %d/100   %s   Gold: %d 🪙\n"
                    hq-level (mod hq-xp 100)
                    (hq-progress-bar (mod hq-xp 100) 100 20)
                    hq-gold))
    (insert (hq-make-divider) "\n")
    (insert (propertize " TASK REWARD " 'face '(:foreground "#5C85D6" :weight bold)) "\n")
    (insert (format " Category: %s\n"
                    (propertize category 'face `(:foreground ,category-color :weight bold))))
    (insert (format " Reward: %s\n"
                    (propertize (format "%d XP, %d gold" potential-xp potential-gold)
                                'face '(:foreground "#4CAF50" :weight bold))))
    (insert " Multipliers:\n")
    (insert (format "   • Priority: ×%.1f\n" priority-mult))
    (insert (format "   • Time Bonus: ×%.1f\n" time-bonus))
    (insert (format "   • Deadline Bonus: ×%.1f\n" deadline-bonus))
    (insert (hq-make-divider ?=) "\n")))

(defun hq-add-quest-info-to-agenda (&optional arg)
  "Add enhanced quest information to the agenda buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (let ((current-level (or hq-level 1))
          (current-xp (or hq-xp 0))
          (current-gold (or hq-gold 0)))
      (goto-char (point-max))
      (insert "\n")
      (insert (hq-make-divider ?=) "\n")
      (insert (hq-make-header "🎮 HABIT QUEST SYSTEM 🎮") "\n")
      (insert (hq-make-divider ?=) "\n")
      (insert (propertize " CHARACTER " 'face '(:foreground "#5C85D6" :weight bold)) "\n")
      (insert (format " Level: %d   XP: %d/100   %s   Gold: %d 🪙\n"
                      current-level (mod current-xp 100)
                      (hq-progress-bar (mod current-xp 100) 100 20)
                      current-gold))
      (when hq-daily-bonus
        (insert (hq-make-divider) "\n")
        (insert (propertize " DAILY BONUS " 'face '(:foreground "#5C85D6" :weight bold)) "\n")
        (insert (format " Complete: %s\n"
                        (propertize (plist-get hq-daily-bonus :habit)
                                    'face '(:foreground "#4ECDC4" :weight bold))))
        (insert (format " Reward: %s\n"
                        (propertize (format "+%d XP, +%d gold"
                                            (or (plist-get hq-daily-bonus :xp) 0)
                                            (or (plist-get hq-daily-bonus :gold) 0))
                                    'face '(:foreground "#4CAF50" :weight bold)))))
      (insert (hq-make-divider) "\n")
      (insert (propertize " ACTIVE QUESTS " 'face '(:foreground "#5C85D6" :weight bold)) "\n")
      (let ((active-quests 0))
        (dolist (quest hq-quests)
          (unless (plist-get quest :completed)
            (setq active-quests (1+ active-quests))
            (let* ((name (or (plist-get quest :name) "Unknown Quest"))
                   (progress (or (plist-get quest :progress) 0))
                   (required (or (plist-get quest :required) 1))
                   (progress-percent (if (> required 0)
                                         (* (/ (float progress) required) 100)
                                       0.0))
                   (quest-icon (cond
                                ((>= progress required) "✅")
                                ((>= progress (/ required 2)) "🔶")
                                (t "🔷"))))
              (insert (format " %s %s\n" quest-icon
                              (propertize name 'face '(:foreground "#4A90E2" :weight bold))))
              (insert (format "   %d/%d days %s %.1f%%\n"
                              progress required
                              (hq-progress-bar progress required 20)
                              progress-percent))
              (insert (format "   Reward: %s\n\n"
                              (propertize (format "+%d XP, +%d gold"
                                                  (or (plist-get quest :reward-xp) 0)
                                                  (or (plist-get quest :reward-gold) 0))
                                          'face '(:foreground "#4CAF50")))))))
        (when (zerop active-quests)
          (insert (propertize " No active quests\n"
                              'face '(:foreground "#888888" :slant italic))))
        (insert (hq-make-divider) "\n")
        (insert (propertize " STORE " 'face '(:foreground "#5C85D6" :weight bold)) "\n")
        (let ((market-button
               (propertize " 🏪 OPEN STORE "
                           'face '(:foreground "white"
                                       :background "#4CAF50"
                                       :weight bold
                                       :box (:line-width 2 :style released-button))
                           'mouse-face 'highlight
                           'keymap (let ((map (make-sparse-keymap)))
                                     (define-key map [mouse-1] 'hq-market)
                                     map))))
          (insert " " market-button "\n"))
        (insert (hq-make-divider ?=) "\n")))))

(defvar hq-market-discount nil "Current discount in the store (percentage).")
(defvar hq-market-discount-duration nil "Duration of the current discount.")
(defvar hq-market-last-refresh nil "Time of the last store refresh.")

(defun hq-market-apply-random-discount ()
  "Apply a random discount to items."
  (let ((discount (nth (random 3) '(10 20 30))))
    (setq hq-market-discount discount)
    (setq hq-market-discount-duration (time-add (current-time) (days-to-time 1)))
    (setq hq-market-last-refresh (current-time))))

(defun hq-market-check-discount ()
  "Check and update the discount status."
  (when (and hq-market-discount-duration
             (time-less-p hq-market-discount-duration (current-time)))
    (setq hq-market-discount nil)
    (setq hq-market-discount-duration nil)))

(defun hq-market-add-to-inventory (item-id)
  "Add an item to the inventory."
  (push item-id hq-inventory)
  (hq-save-data))

(defun hq-market-remove-from-inventory (item-id)
  "Remove an item from the inventory."
  (setq hq-inventory (delete item-id hq-inventory))
  (hq-save-data))

(defun hq-market ()
  "Open an interactive store with purchase and use buttons."
  (interactive)
  (hq-market-check-discount)
  (let ((buffer (get-buffer-create "*Habit Market*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert
         (propertize "🏪 HABIT MARKET\n"
                     'face '(:height 1.5 :weight bold :foreground "#4A90E2"))
         (propertize "============================\n"
                     'face '(:foreground "#4A90E2"))
         (propertize (format "💰 Balance: %d gold\n" hq-gold)
                     'face '(:weight bold)))
        (when hq-market-discount
          (insert
           (propertize (format "🔥 %d%% DISCOUNT! 🔥\n" hq-market-discount)
                       'face '(:foreground "#FF5722" :weight bold))))
        (insert "\n")
        (dolist (category hq-market-categories)
          (let ((category-id (plist-get category :id))
                (category-name (plist-get category :name))
                (category-icon (plist-get category :icon)))
            (insert
             (propertize (format "%s %s\n" category-icon category-name)
                         'face '(:weight bold :height 1.2 :foreground "#2196F3")))
            (dolist (item hq-market-items)
              (when (string= (plist-get item :category) category-id)
                (let* ((item-id (plist-get item :id))
                       (item-name (plist-get item :name))
                       (item-desc (plist-get item :description))
                       (item-cost (plist-get item :cost))
                       (discountable (not (eq (plist-get item :discountable) nil)))
                       (final-cost (if (and hq-market-discount discountable)
                                       (round (* item-cost (- 1 (/ hq-market-discount 100.0))))
                                     item-cost))
                       (in-inventory (member item-id hq-inventory)))
                  (insert "  ")
                  (insert (propertize item-name 'face '(:weight bold)))
                  (insert " - ")
                  (if (and hq-market-discount discountable)
                      (insert (propertize (format "🪙%d→%d" item-cost final-cost)
                                          'face '(:foreground "#FFD700" :weight bold)))
                    (insert (propertize (format "🪙%d" final-cost)
                                        'face '(:foreground "#FFD700" :weight bold))))
                  (insert " ")
                  (if in-inventory
                      (insert-text-button "Use"
                                          'action (lambda (_) (hq-market-use-item item-id))
                                          'follow-link t
                                          'face '(:foreground "white" :background "#4CAF50" :weight bold))
                    (if (>= hq-gold final-cost)
                        (insert-text-button "Buy"
                                            'action (lambda (_) (hq-market-buy-item item-id))
                                            'follow-link t
                                            'face '(:foreground "white" :background "#2196F3" :weight bold))
                      (insert (propertize " ❌ Insufficient gold"
                                          'face '(:foreground "#FF5722")))))
                  (insert "\n    ")
                  (insert (propertize item-desc
                                      'face '(:slant italic :height 0.9 :foreground "#666666")))
                  (insert "\n"))))
            (insert "\n")))
        (insert
         (propertize "📦 Your Inventory\n"
                     'face '(:weight bold :height 1.1)))
        (if hq-inventory
            (dolist (item-id hq-inventory)
              (let* ((item (seq-find (lambda (i)
                                       (string= (plist-get i :id) item-id))
                                     hq-market-items))
                     (item-name (plist-get item :name)))
                (insert "  • " item-name "\n")))
          (insert (propertize "  Inventory is empty\n"
                              'face '(:foreground "#888888" :slant italic)))))
      (special-mode)
      (local-set-key "q" 'quit-window)
      (local-set-key "r" 'hq-market))
    (switch-to-buffer buffer)))

(defun hq-market-buy-item (item-id)
  "Purchase an item from the store."
  (let* ((item (seq-find (lambda (i)
                           (string= (plist-get i :id) item-id))
                         hq-market-items))
         (cost (plist-get item :cost))
         (discountable (not (eq (plist-get item :discountable) nil)))
         (final-cost (if (and hq-market-discount discountable)
                         (round (* cost (- 1 (/ hq-market-discount 100.0))))
                       cost)))
    (if (>= hq-gold final-cost)
        (progn
          (setq hq-gold (- hq-gold final-cost))
          (hq-market-add-to-inventory item-id)
          (message "✨ You purchased %s for %d gold!"
                   (plist-get item :name) final-cost))
      (message "❌ Not enough gold to purchase %s!"
               (plist-get item :name)))
    (hq-market)))

(defun hq-market-use-item (item-id)
  "Use an item from the inventory."
  (let* ((item (seq-find (lambda (i)
                           (string= (plist-get i :id) item-id))
                         hq-market-items))
         (use-message (plist-get item :use-message)))
    (hq-market-remove-from-inventory item-id)
    (message "🎉 %s" use-message)
    (hq-market)))

(defvar hq-daily-bonus nil "Current daily bonus task.")
(defvar hq-last-bonus-date nil "Date of the last bonus task update.")
(defvar habit-stats (make-hash-table :test 'equal)
  "Hash table for storing habit statistics.")

(defun hq-calculate-combined-streak (habits habit-stats)
  "Calculate the number of consecutive days all habits were completed."
  (let ((streaks-data nil))
    (dolist (habit habits)
      (when-let ((habit-data (gethash habit habit-stats)))
        (push (cdr habit-data) streaks-data)))
    (when (= (length streaks-data) (length habits))
      (let ((combined-streak 0)
            (day-index 0)
            (continue t))
        (while (and continue
                    (< day-index (length (car streaks-data))))
          (let ((all-done t))
            (dolist (habit-state streaks-data)
              (when (and (< day-index (length habit-state))
                         (not (char-equal (aref habit-state day-index) ?●)))
                (setq all-done nil)))
            (if all-done
                (setq combined-streak (1+ combined-streak))
              (setq continue nil)))
          (setq day-index (1+ day-index)))
        combined-streak))))

(defun hq-update-quest-progress ()
  "Update quest progress based on current habit streaks."
  (interactive)
  (clrhash habit-stats)
  (with-current-buffer "*Org Agenda*"
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (get-text-property (point) 'org-habit-p)
          (let* ((marker (get-text-property (point) 'org-hd-marker))
                 (habit-name nil)
                 (habit-streak 0)
                 (habits-state nil))
            (when marker
              (with-current-buffer (marker-buffer marker)
                (save-excursion
                  (goto-char (marker-position marker))
                  (setq habit-name (org-get-heading t t t t))))
              (save-excursion
                (end-of-line)
                (when (re-search-backward "\\[🔥 \\([0-9]+\\)\\]" (line-beginning-position) t)
                  (setq habit-streak (string-to-number (match-string 1)))
                  (when (re-search-backward "\\([○●◎]+\\)" (line-beginning-position) t)
                    (setq habits-state (buffer-substring-no-properties
                                        (match-beginning 1)
                                        (match-end 1))))
                  (when (and habit-name habit-streak)
                    (puthash habit-name
                             (cons habit-streak (or habits-state ""))
                             habit-stats))))))
          (forward-line 1)))))
  (dolist (quest hq-quests)
    (unless (plist-get quest :completed)
      (let* ((habits (plist-get quest :habits))
             (required (plist-get quest :required))
             (current-progress 0))
        (if (= (length habits) 1)
            (let ((habit-data (gethash (car habits) habit-stats)))
              (when habit-data
                (setq current-progress (car habit-data))))
          (setq current-progress
                (hq-calculate-combined-streak habits habit-stats)))
        (setf (plist-get quest :progress) current-progress))))
  (hq-save-data))

(defun hq-generate-daily-bonus ()
  "Generate a new daily bonus task."
  (interactive)
  (let* ((current-date (format-time-string "%Y-%m-%d"))
         (all-habits '()))
    (dolist (quest hq-quests)
      (unless (plist-get quest :completed)
        (dolist (habit (plist-get quest :habits))
          (push habit all-habits))))
    (setq all-habits (delete-dups all-habits))
    (when (or (not hq-last-bonus-date)
              (not (string= current-date hq-last-bonus-date)))
      (if all-habits
          (let* ((selected-habit (nth (random (length all-habits)) all-habits))
                 (bonus-xp 40)
                 (bonus-gold 30))
            (setq hq-daily-bonus (list :habit selected-habit
                                       :xp bonus-xp
                                       :gold bonus-gold)))
        (setq hq-daily-bonus nil))
      (setq hq-last-bonus-date current-date)
      (hq-save-data))))

(defun hq-check-daily-bonus (habit-name)
  "Check if a habit is the daily bonus task."
  (when (and hq-daily-bonus
             (string= habit-name (plist-get hq-daily-bonus :habit)))
    (let ((bonus-xp (plist-get hq-daily-bonus :xp))
          (bonus-gold (plist-get hq-daily-bonus :gold)))
      (setq hq-xp (+ hq-xp bonus-xp))
      (setq hq-gold (+ hq-gold bonus-gold))
      (message "Daily bonus task completed! +%d XP, +%d gold!"
               bonus-xp bonus-gold)
      (setq hq-daily-bonus nil)
      (hq-save-data))))

(defun hq-org-habit-streak-hook ()
  "Hook for adding to org-habit-streak-count for integration with the quest system."
  (with-current-buffer "*Org Agenda*"
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (get-text-property (point) 'org-habit-p)
          (let* ((marker (get-text-property (point) 'org-hd-marker))
                 (habit-name nil))
            (when marker
              (with-current-buffer (marker-buffer marker)
                (save-excursion
                  (goto-char (marker-position marker))
                  (setq habit-name (org-get-heading t t t t))))
              (when (and hq-daily-bonus habit-name
                         (string= habit-name (plist-get hq-daily-bonus :habit)))
                (save-excursion
                  (end-of-line)
                  (let ((inhibit-read-only t))
                    (insert " 🌟")))))))
        (forward-line 1)))))

(defun hq-complete-quest (quest-name)
  "Manually complete a quest and claim its reward."
  (interactive
   (list (completing-read "Select a quest to complete: "
                          (mapcar (lambda (quest)
                                    (unless (plist-get quest :completed)
                                      (plist-get quest :name)))
                                  hq-quests)
                          nil t)))
  (let ((quest nil))
    (dolist (q hq-quests)
      (when (and (not (plist-get q :completed))
                 (string= (plist-get q :name) quest-name))
        (setq quest q)))
    (if quest
        (progn
          (setf (plist-get quest :completed) t)
          (let ((reward-xp (plist-get quest :reward-xp))
                (reward-gold (plist-get quest :reward-gold)))
            (setq hq-xp (+ hq-xp reward-xp))
            (setq hq-gold (+ hq-gold reward-gold))
            (let ((new-level (1+ (/ hq-xp 100))))
              (when (> new-level hq-level)
                (setq hq-level new-level)
                (message "Level up! You are now level %d!" hq-level)))
            (message "Quest completed: %s! +%d XP, +%d gold!"
                     quest-name reward-xp reward-gold)))
      (message "Quest not found or already completed")))
  (hq-save-data))

(defun hq-habits-quest-view ()
  "Display the agenda with quest information."
  (interactive)
  (unless hq-quests
    (setq hq-quests '()))
  (hq-generate-daily-bonus)
  (condition-case err
      (org-agenda nil "x")
    (error
     (message "Error opening agenda: %s" err)))
  (run-with-timer 0.5 nil
                  (lambda ()
                    (when (get-buffer "*Org Agenda*")
                      (with-current-buffer "*Org Agenda*"
                        (save-excursion
                          (condition-case nil
                              (progn
                                (hq-update-quest-progress)
                                (let ((inhibit-read-only t))
                                  (hq-add-quest-info-to-agenda)))
                            (error
                             (message "Error updating quest information")))))))))

(defun hq-update-quest-info ()
  "Update quest information in the agenda."
  (when (string= (buffer-name) "*Org Agenda*")
    (message "Updating quest progress...")
    (hq-update-quest-progress)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-max))
        (hq-add-quest-info-to-agenda)))))

(defun hq-setup ()
  "Set up the habit quest system."
  (interactive)
  (hq-load-data)
  (message "Habit quest system for org-habit is set up!"))

;;; Custom Functions
(defun org-agenda-refresh ()
  "Refresh all `org-agenda' buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-agenda-mode)
        (org-agenda-maybe-redo)))))

(defun org-habit-count-last-streak (state-str)
  "Count consecutive completed days (●), including incomplete tasks (◎)."
  (let ((streak 0)
        (length (length state-str))
        (has-completed nil))
    (catch 'break
      (dotimes (i length)
        (let ((current-char (aref state-str (- length i 1))))
          (cond
           ((char-equal current-char ?◎)
            (when has-completed
              (setq streak (1+ streak))))
           ((char-equal current-char ?●)
            (setq streak (1+ streak))
            (setq has-completed t))
           (t
            (throw 'break streak))))))
    streak))

(defun org-habit-streak-count ()
  "Display current streak for each habit in org-agenda."
  (goto-char (point-min))
  (while (not (eobp))
    (when (get-text-property (point) 'org-habit-p)
      (let ((streak 0))
        (save-excursion
          (when (re-search-forward "\\([○●◎]\\)+" (line-end-position) t)
            (let ((state-str (match-string 0)))
              (setq streak (org-habit-count-last-streak state-str)))))
        (end-of-line)
        (insert (format " [🔥 %d]" streak))))
    (forward-line 1)))

(defun my-find-work-habit ()
  "Find the '3+ hours of work' habit in org files and return its data."
  (let ((work-habit-data nil))
    (dolist (file (org-agenda-files))
      (with-current-buffer (find-file-noselect file)
        (org-with-point-at 1
          (while (and (not work-habit-data)
                      (re-search-forward "⚡ - 3\\+ hours of work" nil t))
            (let ((pos (point)))
              (org-back-to-heading t)
              (when (org-is-habit-p)
                (setq work-habit-data
                      (org-habit-stats-parse-todo (point))))
              (goto-char pos))))))
    work-habit-data))

(defun my-display-work-habit-calendar ()
  "Display a calendar for the '3+ hours of work' habit at the start of the org-agenda buffer."
  (let ((work-habit-data (my-find-work-habit)))
    (when work-habit-data
      (org-habit-stats-make-calendar-buffer work-habit-data)
      (save-excursion
        (goto-char (point-min))
        (when (search-forward "Everytime" nil t)
          (forward-line -1)
          (insert "\nWork Hours Calendar (3+ hours per day)\n")
          (insert "================================================================\n")
          (let ((calendar-content (with-current-buffer org-habit-stats-calendar-buffer
                                    (buffer-string)))
                (calendar-overlays (org-habit-stats-get-calendar-overlays)))
            (let ((start-pos (point)))
              (insert calendar-content)
              (org-habit-stats-apply-overlays calendar-overlays
                                              (- start-pos 1)
                                              (current-buffer))))
          (insert "\n================================================================\n"))))))

;; Hooks and Keybindings
(add-hook 'org-agenda-finalize-hook 'org-habit-streak-count)
(add-hook 'org-agenda-finalize-hook 'my-display-work-habit-calendar)
(global-set-key (kbd "C-c q") 'hq-habits-quest-view)
(global-set-key (kbd "C-c m") 'hq-market)

;; Initialize the system
(hq-setup)

(provide 'better-org-habit)
;;; better-org-habit.el ends here
