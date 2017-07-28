;;; -*- Mode: emacs-lisp -*-

(setq gc-cons-threshold (* 1024 1024 16))

;;;
;;; Configure basic UI settings
;;;

(setq inhibit-startup-message t)
(setq initial-major-mode 'emacs-lisp-mode)
(setq initial-scratch-message nil)
(setq ring-bell-function 'ignore)
(setq echo-keystrokes 0.1)
(setq confirm-kill-emacs 'y-or-n-p)
(setq scroll-conservatively 1)
(setq next-line-add-newlines t)


(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil)


;;;
;;; Configure environment settings
;;;

;;; macOS specific settings
(when (eq window-system 'mac)
  (set-default-font "Menlo")
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'option)
  ;;(setq mac-function-modifier 'hyper)
  (setenv "LANG" "en_US.UTF-8"))

;;; Windows specific settings
(when (eq window-system 'w32)
  (set-face-attribute 'default nil :font "Consolas 11")
  (setq-default default-directory (file-name-as-directory (getenv "HOMEPATH"))))

;;; Setup proxy on my work machine
(when (string-match-p "C171*" system-name)
  (setq url-proxy-services
        '(("no_proxy" . "^\\(localhost\\|10.*\\)")
          ("http" . "localhost:3128")
          ("https" . "localhost:3128"))))

(add-to-list 'load-path "~/.emacs.d/lisp")

;;;
;;; Setup default modes
;;;

;; Disable scrollbars, the toolbar and blinking cursor
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (blink-cursor-mode -1)
  ;;(menu-bar-mode -1)
  )

(delete-selection-mode 1)
(show-paren-mode 1)
(column-number-mode 1)
(global-visual-line-mode 1)
(global-hl-line-mode 1)
(winner-mode 1)


;;;
;;; Setup some global keybindings
;;; 

(global-set-key (kbd "\C-x k") 'kill-this-buffer)
(global-set-key [escape] 'keyboard-escape-quit)
(define-key isearch-mode-map [escape] 'isearch-cancel)

(windmove-default-keybindings)


;;; 
;;; Configure backups and auto-saves
;;; 

(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      backup-directory-alist '(("." . "~/.emacs.d/backups"))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))


;;;
;;; Setup the packaging system
;;;
;;; Add MELPA to the list of package repositories, initialize the
;;; system and make sure that USE-PACKAGE is installed.  All
;;; subsequent package definitions make use of it.
;;; 
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;;;
;;; Package definitions
;;;

;;; UI

(use-package doom-themes
  :ensure t
  :init
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (doom-themes-neotree-config)
  :config
  (load-theme 'doom-one 'no-confirm))

(use-package solaire-mode
  :ensure t
  :init
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode))

(use-package nlinum
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'nlinum-mode)
  (setq nlinum-format "%4d "))

(use-package nlinum-hl
  :ensure t)

(use-package spaceline-all-the-icons
  :ensure t
  :init
  (spaceline-all-the-icons-theme)
  (spaceline-all-the-icons--setup-anzu)
  (spaceline-all-the-icons--setup-neotree))

(use-package neotree
  :ensure t
  :init
  (setq neo-auto-indent-point t)
  (setq neo-hidden-regexp-list '("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "\\.fasl"))
  (setq neo-theme 'nerd)
  (setq neo-window-fixed-size nil)
  (setq neo-window-width 30)
  (setq neo-mode-line-type 'none)
  :bind
  (("C-c n" . neotree)))



;;; Global modes

(use-package diminish
  :ensure t
  :diminish visual-line-mode
  :diminish eldoc-mode
  :diminish auto-revert-mode)

(use-package ace-jump-mode
  :ensure t
  :bind
  (("C-c SPC" . ace-jump-mode)))

(use-package ace-jump-zap
  :ensure t
  :bind
  (("C-c z" . ace-jump-zap-up-to-char)))

(use-package ace-window
  :ensure t
  :bind
  (("M-p" . ace-window)))

(use-package helm
  :ensure t
  :diminish helm-mode
  :init 
  (require 'helm-config)
  (setq helm-split-window-in-side-p t)
  (setq helm-display-header-line nil)
  (setq helm-ff-skip-boring-files t)
  (setq helm-boring-file-regexp-list
	'("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "~$"
	  "\\.so$" "\\.a$" "\\.elc$" "\\.fas$" "\\.fasl$" "\\.pyc$" "\\.pyo$"
          "\\.dx64fsl"))
  (helm-mode)
  :bind
  (("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-x b" . helm-mini)
   ("C-x C-f" . helm-find-files)
   ("M-o" . helm-occur)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   ("C-i" . helm-execute-persistent-action)
   ("C-z" . helm-select-action)))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode))

(use-package company
  :ensure t
  :diminish company-mode
  :bind
  (("M-/" . company-complete)
   :map company-active-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("C-d" . company-show-doc-buffer))
  :init
  (setq company-tooltip-idle-delay 0.3)
  (setq company-tooltip-limit 20)
  (add-hook 'prog-mode-hook 'global-company-mode))

(use-package company-web
  :ensure t)

(use-package window-numbering
  :ensure t
  :init
  (window-numbering-mode))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (which-key-mode))

(use-package anzu
  :ensure t
  :diminish anzu-mode
  :init
  (global-anzu-mode))

(use-package beacon
  :ensure t
  :init
  (beacon-mode))

(use-package dash-at-point
  :ensure t
  :bind
  (("C-c d" . dash-at-point)))

;;; Programming modes

(let ((slime-helper (expand-file-name "~/.roswell/helper.el")))
  (when (file-exists-p slime-helper)
    (load slime-helper)))

(use-package slime 
  :config
  (require 'slime-company)
  
  (setq slime-net-coding-system 'utf-8-unix)
  (setq inferior-lisp-program "ros -Q run")
  (setq slime-contribs '(slime-fancy slime-listener-hooks slime-indentation))

  ;; (define-common-lisp-style "omarkov"
  ;;   "My own style."
  ;;   (:inherit "modern")
  ;;   (:variables
  ;;    (lisp-loop-indent-subclauses t))
  ;;   (:indentation
  ;;    (make-instance (2 &rest 2))))

  ;; (setq common-lisp-style-default "omarkov")
  
  (put :default-initargs 'common-lisp-indent-function '(&rest)))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode))

(use-package web-mode
  :ensure t
  :mode "\\.html\\'"
  :config
  (add-hook 'web-mode-hook
	    (lambda () 
	      (add-to-list (make-local-variable 'company-backends) 'company-web-html))))

(use-package js2-mode
  :ensure t
  :defer t)

(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :config
  (add-hook 'prog-mode-hook 'aggressive-indent-mode))



;;; org-mode


(use-package org
  :ensure t
  :preface
  (defun om/open-org-files ()
    (interactive)
    (require 'org) ; i'm sure there's a better way
    (let ((org-files '("work.org" "personal.org")))
      (mapcar (lambda (f) (find-file (concat org-directory "/" f))) org-files)))

  (defun bh/find-project-task ()
    "Move point to the parent (project) task if any"
    (save-restriction
      (widen)
      (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (goto-char parent-task)
        parent-task)))
  (defun bh/is-project-p ()
    "Any task with a todo keyword subtask"
    (save-restriction
      (widen)
      (let ((has-subtask)
            (subtree-end (save-excursion (org-end-of-subtree t)))
            (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-todo-keywords-1)
              (setq has-subtask t))))
        (and is-a-task has-subtask))))

  (defun bh/is-project-subtree-p ()
    "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
    (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                                (point))))
      (save-excursion
        (bh/find-project-task)
        (if (equal (point) task)
            nil
          t))))

  (defun bh/is-task-p ()
    "Any task with a todo keyword and no subtask"
    (save-restriction
      (widen)
      (let ((has-subtask)
            (subtree-end (save-excursion (org-end-of-subtree t)))
            (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-todo-keywords-1)
              (setq has-subtask t))))
        (and is-a-task (not has-subtask)))))

  (defun bh/is-subproject-p ()
    "Any task which is a subtask of another project"
    (let ((is-subproject)
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (while (and (not is-subproject) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq is-subproject t))))
      (and is-a-task is-subproject)))

  (defun bh/list-sublevels-for-projects-indented ()
    "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
    (if (marker-buffer org-agenda-restrict-begin)
        (setq org-tags-match-list-sublevels 'indented)
      (setq org-tags-match-list-sublevels nil))
    nil)

  (defun bh/list-sublevels-for-projects ()
    "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
    (if (marker-buffer org-agenda-restrict-begin)
        (setq org-tags-match-list-sublevels t)
      (setq org-tags-match-list-sublevels nil))
    nil)

  (defvar bh/hide-scheduled-and-waiting-next-tasks t)

  (defun bh/toggle-next-task-display ()
    (interactive)
    (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
    (when  (equal major-mode 'org-agenda-mode)
      (org-agenda-redo))
    (message "%s WAITING and SCHEDULED NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

  (defun bh/skip-stuck-projects ()
    "Skip trees that are not stuck projects"
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (if (bh/is-project-p)
            (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                   (has-next ))
              (save-excursion
                (forward-line 1)
                (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                  (unless (member "WAITING" (org-get-tags-at))
                    (setq has-next t))))
              (if has-next
                  nil
                next-headline)) ; a stuck project, has subtasks but no next task
          nil))))

  (defun bh/skip-non-stuck-projects ()
    "Skip trees that are not stuck projects"
    ;; (bh/list-sublevels-for-projects-indented)
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (if (bh/is-project-p)
            (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                   (has-next ))
              (save-excursion
                (forward-line 1)
                (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                  (unless (member "WAITING" (org-get-tags-at))
                    (setq has-next t))))
              (if has-next
                  next-headline
                nil)) ; a stuck project, has subtasks but no next task
          next-headline))))

  (defun bh/skip-non-projects ()
    "Skip trees that are not projects"
    ;; (bh/list-sublevels-for-projects-indented)
    (if (save-excursion (bh/skip-non-stuck-projects))
        (save-restriction
          (widen)
          (let ((subtree-end (save-excursion (org-end-of-subtree t))))
            (cond
             ((bh/is-project-p)
              nil)
             ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
              nil)
             (t
              subtree-end))))
      (save-excursion (org-end-of-subtree t))))

  (defun bh/skip-non-tasks ()
    "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (cond
         ((bh/is-task-p)
          nil)
         (t
          next-headline)))))

  (defun bh/skip-project-trees-and-habits ()
    "Skip trees that are projects"
    (save-restriction
      (widen)
      (let ((subtree-end (save-excursion (org-end-of-subtree t))))
        (cond
         ((bh/is-project-p)
          subtree-end)
         ((org-is-habit-p)
          subtree-end)
         (t
          nil)))))

  (defun bh/skip-projects-and-habits-and-single-tasks ()
    "Skip trees that are projects, tasks that are habits, single non-project tasks"
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (cond
         ((org-is-habit-p)
          next-headline)
         ((and bh/hide-scheduled-and-waiting-next-tasks
               (member "WAITING" (org-get-tags-at)))
          next-headline)
         ((bh/is-project-p)
          next-headline)
         ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
          next-headline)
         (t
          nil)))))

  (defun bh/skip-project-tasks-maybe ()
    "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
    (save-restriction
      (widen)
      (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
             (next-headline (save-excursion (or (outline-next-heading) (point-max))))
             (limit-to-project (marker-buffer org-agenda-restrict-begin)))
        (cond
         ((bh/is-project-p)
          next-headline)
         ((org-is-habit-p)
          subtree-end)
         ((and (not limit-to-project)
               (bh/is-project-subtree-p))
          subtree-end)
         ((and limit-to-project
               (bh/is-project-subtree-p)
               (member (org-get-todo-state) (list "NEXT")))
          subtree-end)
         (t
          nil)))))

  (defun bh/skip-project-tasks ()
    "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
    (save-restriction
      (widen)
      (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
        (cond
         ((bh/is-project-p)
          subtree-end)
         ((org-is-habit-p)
          subtree-end)
         ((bh/is-project-subtree-p)
          subtree-end)
         (t
          nil)))))

  (defun bh/skip-non-project-tasks ()
    "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
    (save-restriction
      (widen)
      (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
             (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (cond
         ((bh/is-project-p)
          next-headline)
         ((org-is-habit-p)
          subtree-end)
         ((and (bh/is-project-subtree-p)
               (member (org-get-todo-state) (list "NEXT")))
          subtree-end)
         ((not (bh/is-project-subtree-p))
          subtree-end)
         (t
          nil)))))

  (defun bh/skip-projects-and-habits ()
    "Skip trees that are projects and tasks that are habits"
    (save-restriction
      (widen)
      (let ((subtree-end (save-excursion (org-end-of-subtree t))))
        (cond
         ((bh/is-project-p)
          subtree-end)
         ((org-is-habit-p)
          subtree-end)
         (t
          nil)))))

  (defun bh/skip-non-subprojects ()
    "Skip trees that are not projects"
    (let ((next-headline (save-excursion (outline-next-heading))))
      (if (bh/is-subproject-p)
          nil
        next-headline)))
  
  (defun bh/skip-non-archivable-tasks ()
    "Skip trees that are not available for archiving"
    (save-restriction
      (widen)
      ;; Consider only tasks with done todo headings as archivable candidates
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
            (subtree-end (save-excursion (org-end-of-subtree t))))
        (if (member (org-get-todo-state) org-todo-keywords-1)
            (if (member (org-get-todo-state) org-done-keywords)
                (let* ((daynr (string-to-int (format-time-string "%d" (current-time))))
                       (a-month-ago (* 60 60 24 (+ daynr 1)))
                       (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                       (this-month (format-time-string "%Y-%m-" (current-time)))
                       (subtree-is-current (save-excursion
                                             (forward-line 1)
                                             (and (< (point) subtree-end)
                                                  (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                  (if subtree-is-current
                      subtree-end ; Has a date in this month or last month, skip it
                    nil))  ; available to archive
              (or subtree-end (point-max)))
          next-headline))))

  (defun bh/widen ()
    (interactive)
    (if (equal major-mode 'org-agenda-mode)
        (progn
          (org-agenda-remove-restriction-lock)
          (when org-agenda-sticky
            (org-agenda-redo)))
      (widen)))
  
  (defun bh/get-pom-from-agenda-restriction-or-point ()
    (or (org-get-at-bol 'org-hd-marker)
        (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
        (and (equal major-mode 'org-mode) (point))
        org-clock-marker))

  (defun bh/narrow-to-org-subtree ()
    (widen)
    (org-narrow-to-subtree)
    (save-restriction
      (org-agenda-set-restriction-lock)))
  
  (defun bh/narrow-to-org-project ()
    (widen)
    (save-excursion
      (bh/find-project-task)
      (bh/narrow-to-org-subtree)))

  (defun bh/narrow-to-project ()
    (interactive)
    (if (equal major-mode 'org-agenda-mode)
        (progn
          (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
            (bh/narrow-to-org-project)
            (save-excursion
              (bh/find-project-task)
              (org-agenda-set-restriction-lock)))
          (org-agenda-redo)
          (beginning-of-buffer))
      (bh/narrow-to-org-project)
      (save-restriction
        (org-agenda-set-restriction-lock))))
  
  (defun bh/narrow-up-one-org-level ()
    (widen)
    (save-excursion
      (outline-up-heading 1 'invisible-ok)
      (bh/narrow-to-org-subtree)))
  
  (defun bh/narrow-up-one-level ()
    (interactive)
    (if (equal major-mode 'org-agenda-mode)
        (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
          (bh/narrow-up-one-org-level))
      (bh/narrow-up-one-org-level)))
  :bind
  (("C-c c" . org-capture)
   ("C-c a" . org-agenda)
   ("C-c o" . om/open-org-files))
  :config
  ;; Files & Directories
  (setq org-directory (expand-file-name "~/org"))
  (setq org-agenda-files '("~/org"))
  (setq org-default-notes-file (concat org-directory "/capture.org"))

  ;; UI
  (setq org-startup-indented t)
  (setq org-tags-column 85)
  (setq org-agenda-tags-column 88)
  (setq org-fontify-whole-heading-line t)
  (setq org-hide-emphasis-markers t)
  (setq org-hide-leading-stars t)
  (setq org-odd-levels-only nil)
  (setq org-time-stamp-custom-formats '("<%e %b, %Y>" .
                                        "<%e %b, %Y %H:%M>"))
  
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t)

  ;; Behaviour
  (setq org-log-done t)
  (setq org-use-fast-todo-selection t)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)
  (setq org-enforce-todo-dependencies t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w)" "HOLD(h)" "|" "CANCELLED(c)"))
        org-todo-repeat-to-state "NEXT")

  (setq org-todo-keyword-faces
        '(("NEXT" :inherit font-lock-function-name-face :weight bold)
          ("WAITING" :inherit warning)))

  (setq org-todo-state-tags-triggers
        '(("CANCELLED" ("CANCELLED" . t))
          ("WAITING" ("WAITING" . t))
          ("HOLD" ("WAITING") ("HOLD" . t))
          (done ("WAITING") ("HOLD"))
          ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
          ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
          ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
  
  (setq org-special-ctrl-a/e t
        org-special-ctrl-k t)
  (setq org-yank-adjusted-subtrees t)
  
  ;; Agenda
  (setq org-stuck-projects (quote ("" nil nil "")))

  (setq org-agenda-compact-blocks t
        org-agenda-sticky t
        org-agenda-start-on-weekday nil
        ;;org-agenda-span 'day
        org-agenda-include-diary nil
        org-agenda-sorting-strategy
        '((agenda habit-down time-up user-defined-up effort-up category-keep)
          (todo category-up effort-up)
          (tags category-up effort-up)
          (search category-up))
        org-agenda-window-setup 'current-window
        org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-custom-commands
        '(("N" "Notes" tags "NOTE"
           ((org-agenda-overriding-header "Notes")
            (org-tags-match-list-sublevels t)))
          ("h" "Habits" tags-todo "STYLE=\"habit\""
           ((org-agenda-overriding-header "Habits")
            (org-agenda-sorting-strategy
             '(todo-state-down effort-up category-keep))))
          ("g" "Agenda"
           ((agenda "" nil)
            (tags "refile"
                  ((org-agenda-overriding-header "Tasks to Refile")
                   (org-tags-match-list-sublevels nil)))
            (tags-todo "-CANCELLED/!"
                       ((org-agenda-overriding-header "Stuck Projects")
                        (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-HOLD-CANCELLED/!"
                       ((org-agenda-overriding-header "Projects")
                        (org-agenda-skip-function 'bh/skip-non-projects)
                        (org-tags-match-list-sublevels 'indented)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-CANCELLED/!NEXT"
                       ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                              (if bh/hide-scheduled-and-waiting-next-tasks
                                                                  ""
                                                                " (including WAITING and SCHEDULED tasks)")))
                        (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                        (org-tags-match-list-sublevels t)
                        (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-sorting-strategy
                         '(todo-state-down effort-up category-keep))))
            (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                       ((org-agenda-overriding-header (concat "Project Subtasks"
                                                              (if bh/hide-scheduled-and-waiting-next-tasks
                                                                  ""
                                                                " (including WAITING and SCHEDULED tasks)")))
                        (org-agenda-skip-function 'bh/skip-non-project-tasks)
                        (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                       ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                              (if bh/hide-scheduled-and-waiting-next-tasks
                                                                  ""
                                                                " (including WAITING and SCHEDULED tasks)")))
                        (org-agenda-skip-function 'bh/skip-project-tasks)
                        (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-CANCELLED+WAITING|HOLD/!"
                       ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                              (if bh/hide-scheduled-and-waiting-next-tasks
                                                                  ""
                                                                " (including WAITING and SCHEDULED tasks)")))
                        (org-agenda-skip-function 'bh/skip-non-tasks)
                        (org-tags-match-list-sublevels nil)
                        (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
            (tags "-REFILE/"
                  ((org-agenda-overriding-header "Tasks to Archive")
                   (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                   (org-tags-match-list-sublevels nil))))
           nil)))

  (add-hook 'org-agenda-mode-hook
            '(lambda () (org-defkey org-agenda-mode-map "W" (lambda () (interactive) (setq bh/hide-scheduled-and-waiting-next-tasks t) (bh/widen))))
            'append)

  (add-hook 'org-agenda-mode-hook
            '(lambda () (org-defkey org-agenda-mode-map "N" 'bh/narrow-to-subtree))
            'append)
  
  (add-hook 'org-agenda-mode-hook
            '(lambda () (org-defkey org-agenda-mode-map "P" 'bh/narrow-to-project))
            'append)
  
  (add-hook 'org-agenda-mode-hook
            '(lambda () (org-defkey org-agenda-mode-map "U" 'bh/narrow-up-one-level))
            'append)
  
  ;; Capture
  (setq org-capture-templates
        '(("t" "Todo" entry (file "~/org/capture.org")
           "* TODO %?")
          ("n" "Note" entry (file"~/org/capture.org")
           "* %?")))

  ;; Configure MS Outlook integration
  ;; FIXME: this isn't too nice
  (when (eq window-system 'w32)
    (require 'org-outlook)
    (setq org-outlook-location (w32-short-file-name
                                "C:\\Program Files (x86)\\Microsoft Office\\Office14\\outlook.exe"))
    (add-to-list
     'org-capture-templates
     '("o" "org-outlook" entry (file "~/org/capture.org" "Tasks")
       "* TODO %?\n  %c\n  %i"))))

(use-package org-brain
  :ensure t
  :bind
  (("C-c v" . org-brain-visualize)))


;;;
;;; Variables set by Emacs customization facility
;;; 

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("19f68ed86c05e6810925c2985f873f7ad728079ade01f5844d7d61e82dcbae4a" "5310b88333fc64c0cb34a27f42fa55ce371438a55f02ac7a4b93519d148bd03d" "67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" "e91ca866d6cbb79786e314e0466f4f1b8892b72e77ed702e53bf7565e0dfd469" default)))
 '(elfeed-feeds (quote ("http://planet.emacsen.org/atom.xml")))
 '(neo-auto-indent-point t)
 '(neo-hidden-regexp-list
   (quote
    ("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "\\.fasl")))
 '(neo-theme (quote nerd))
 '(neo-window-fixed-size t)
 '(neo-window-width 35)
 '(org-agenda-files (quote ("~/test.org")))
 '(org-blank-before-new-entry (quote ((heading . auto) (plain-list-item . auto))))
 '(org-fontify-whole-heading-line t)
 '(org-hide-emphasis-markers t)
 '(org-hide-leading-stars t)
 '(org-level-color-stars-only nil)
 '(org-odd-levels-only t)
 '(org-time-stamp-custom-formats (quote ("<%e %b, %Y>" . "<%e %b, %Y %H:%M>")))
 '(package-selected-packages
   (quote
    (dired-collapse spaceline-all-the-icons kubernetes terraform-mode markdown-mode helm-org-rifle org-brain nlinum-hl solaire-mode elpy eldoc-eval nlinum doom-themes elfeed beacon helm-ag helm-dash helm-mode-manager glsl-mode ace-jump-zap ace-window avy ace-jump-mode dash-at-point move-text groovy-mode gradle-mode yaml-mode helm-descbinds dired+ page-break-lines fill-column-indicator helm-company neotree company-web company-restclient ob-restclient restclient anzu js2-mode json-mode web-mode use-package spaceline zenburn-theme yasnippet window-numbering which-key undo-tree slime-company popup paredit multiple-cursors magit helm-projectile expand-region aggressive-indent)))
 '(safe-local-variable-values
   (quote
    ((Package . IMAGES)
     (Syntax . Zetalisp)
     (Lowercase . T)
     (Package . DIS)
     (Syntax . Common-Lisp)
     (Package . DYNAMIC-WINDOWS)
     (Lowercase . Yes)
     (Base . 10)
     (Package . DW)
     (Syntax . Common-lisp)
     (Package . CCL))))
 '(spaceline-all-the-icons-highlight-file-name t)
 '(spaceline-all-the-icons-separator-type (quote none))
 '(spaceline-all-the-icons-slim-render t))

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(fringe ((t (:background "#3F3F3F" :foreground "#DCDCCC"))))
;;  '(helm-source-header ((t (:background "#2B2B2B" :foreground "#F0DFAF" :box nil :underline nil :weight bold))))
;;  '(linum ((t (:background "#3F3F3F" :foreground "#7f7f7f" :height 0.8))))
;;  '(mode-line ((t (:background "#2B2B2B" :foreground "#8FB28F" :box nil))))
;;  '(mode-line-highlight ((t (:box nil))))
;;  '(mode-line-inactive ((t (:background "#383838" :foreground "#5F7F5F" :box nil))))
;;  '(neo-dir-link-face ((t (:inherit font-lock-function-name-face))))
;;  '(neo-file-link-face ((t nil)))
;;  '(org-block-background ((t (:background "#3A3A3A")))))


;;;
;;; Custom functions
;;;

;;; jump to beginning of line
(define-key global-map [remap move-beginning-of-line]
  (defun smart-beginning-of-line ()
    "Move point to first non-whitespace character or beginning-of-line.
  Move point to the first non-whitespace character on this line.
  If point was already at that position, move point to beginning of line."
    (interactive)
    (let ((oldpos (point)))
      (back-to-indentation)
      (and (= oldpos (point))
           (beginning-of-line)))))

;;; auto-indent after yank
(defadvice insert-for-yank-1 (after indent-region activate)
  "Indent yanked region in certain modes, C-u prefix to disable"
  (if (and (not current-prefix-arg)
           (member major-mode '(sh-mode
                                emacs-lisp-mode lisp-mode
                                c-mode c++-mode objc-mode d-mode java-mode cuda-mode
                                LaTeX-mode TeX-mode
                                xml-mode html-mode css-mode)))
      (indent-region (region-beginning) (region-end) nil)))

(defun start-or-switch-to (function buffer-name)
  "Invoke FUNCTION if there is no buffer with BUFFER-NAME.
Otherwise switch to the buffer named BUFFER-NAME.  Don't clobber
the current buffer."
  (if (not (get-buffer buffer-name))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (funcall function))
    (switch-to-buffer-other-window buffer-name)))

(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (start-or-switch-to (lambda ()
			(ansi-term (getenv "SHELL")))
                      "*ansi-term*"))


(global-set-key (kbd "H-t") 'visit-term-buffer)
(global-set-key (kbd "C-c t") 'visit-term-buffer)
(global-set-key (kbd "H-r") 'slime-repl)

;;; enable some disabled
(put 'erase-buffer 'disabled nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#21242b" :foreground "#bbc2cf" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Menlo"))))
 '(mode-line ((t (:background "#282c34" :box (:line-width 4 :color "#282c34")))))
 '(mode-line-inactive ((t (:background "#1d2026" :foreground "#545668" :box (:line-width 4 :color "#1d2026")))))
 '(solaire-mode-line-face ((t (:inherit mode-line :background "#1c1f25" :box (:line-width 4 :color "#1c1f25")))))
 '(solaire-mode-line-inactive-face ((t (:inherit mode-line-inactive :background "#21242b" :box (:line-width 4 :color "#21242b"))))))
