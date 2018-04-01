;;; -*- Mode: emacs-lisp -*-


;;; ---------------------------------------------------------------------------
;;; Startup settings
;;; ---------------------------------------------------------------------------

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)


;;; ---------------------------------------------------------------------------
;;; Configure environment settings
;;; ---------------------------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/lisp")

(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      backup-directory-alist '(("." . "~/.emacs.d/backups"))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))


;;;
;;; MacOS
;;; 

(when (eq window-system 'mac)
  (set-default-font "Menlo")
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'option)
  (setenv "LANG" "en_US.UTF-8"))

;;; 
;;; Windows
;;;

(when (eq window-system 'w32)
  (set-face-attribute 'default nil :font "Consolas 11")
  (setq-default default-directory (file-name-as-directory (getenv "HOMEPATH")))

  ;;; Setup proxy on my work machine
  (when (string-match-p "CSTRL*" (system-name))
    (setq url-proxy-services
          '(("no_proxy" . "^\\(localhost\\|10.*\\)")
            ("http" . "localhost:3128")
            ("https" . "localhost:3128")))))


;;; ---------------------------------------------------------------------------
;;; General editing behaviour
;;; ---------------------------------------------------------------------------

(setq-default indent-tabs-mode nil)
(setq next-line-add-newlines t)

;;;
;;; Setup default modes
;;;

(delete-selection-mode 1)
(show-paren-mode 1)
(column-number-mode 1)
(global-hl-line-mode 1)

(global-auto-revert-mode 1)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;;;
;;; Setup some global keybindings
;;; 

(global-set-key (kbd "\C-x k") 'kill-this-buffer)
(global-set-key [escape] 'keyboard-escape-quit)
(define-key isearch-mode-map [escape] 'isearch-cancel)

(windmove-default-keybindings)


;;; ---------------------------------------------------------------------------
;;; Setup the packaging system
;;;
;;; Add MELPA to the list of package repositories, initialize the
;;; system and make sure that USE-PACKAGE is installed.  All
;;; subsequent package definitions make use of it.
;;; ---------------------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;;; ---------------------------------------------------------------------------
;;; UI Setup
;;;
;;; Customize visual settings.  Loads the Doom One theme with solaire mode.
;;; Also includes the mode-line from Doom Emacs.
;;; ---------------------------------------------------------------------------

;; Disable scrollbars, the toolbar and blinking cursor
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (blink-cursor-mode -1))

(setq ring-bell-function 'ignore)
(setq echo-keystrokes 0.1)
(setq scroll-conservatively 1)

(defalias 'yes-or-no-p 'y-or-n-p)


;;; 
;;; Configure theme
;;; 

(use-package doom-themes
  :ensure t
  :preface
  ;; Hides the fringe in the minibuffer for a cleaner look
  (defun om/hide-fringe-in-minibuffer ()
    (set-window-fringes (minibuffer-window) 0 0))

  (add-hook 'minibuffer-setup-hook #'om/hide-fringe-in-minibuffer)
  (om/hide-fringe-in-minibuffer) 
  :init
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t) 
  :config
  (doom-themes-neotree-config)
  (doom-themes-org-config) 
  (load-theme 'doom-one 'no-confirm)
  (require 'doom-modeline))

;; brighten buffers (that represent real files) 
(use-package solaire-mode
  :ensure t
  :requires doom-themes
  :hook (after-change-major-mode . turn-on-solaire-mode))


;;;
;;; Configure additional UI packages
;;; 

;;; Display line numbers in programming modes (disabled)
(use-package nlinum
  :disabled t
  :ensure t
  :init
  (setq nlinum-format "%4d ")
  :hook (prog-mode . nlinum-mode))

;;; Display a file tree (bound to C-c n)
(use-package neotree
  :ensure t
  :config
  (setq neo-auto-indent-point t)
  (setq neo-hidden-regexp-list '("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "\\.fasl"))
  (setq neo-theme 'nerd)
  (setq neo-window-fixed-size nil)
  (setq neo-window-width 30)
  (setq neo-mode-line-type 'none)
  (setq neo-smart-open t)
  :hook (neotree-mode . (lambda () (toggle-truncate-lines 1)))
  :bind
  (("C-c n" . neotree)))

(use-package anzu
  :ensure t
  :diminish anzu-mode
  :init
  (setq anzu-cons-mode-line-p nil) ; required to work with doom-modeline
  :config
  (global-anzu-mode))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package beacon
  :ensure t
  :config
  (beacon-mode))

(use-package diminish
  :ensure t
  :diminish visual-line-mode
  :diminish eldoc-mode
  :diminish auto-revert-mode)


;;; ---------------------------------------------------------------------------
;;; Dired
;;; ---------------------------------------------------------------------------

(use-package dired
  :init
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-alh"))

;;; Use dired-x for its omit-mode
(use-package dired-x
  :init
  (setq-default dired-omit-files-p t)
  (setq dired-omit-files "^\\...+$")
  :bind
  (:map dired-mode-map
        (")" . dired-omit-mode)))

(use-package dired-collapse
  :ensure t
  :hook ((dired-mode . dired-collapse-mode)
         (dired-mode . (lambda () (toggle-truncate-lines 1)))))

(use-package dired-k
  :ensure t
  :hook ((dired-initial-position . dired-k)
         (dired-after-readin . dired-k-no-revert))
  :init
  (setq dired-k-style 'git)
  (setq dired-k-human-readable t))

(use-package dired-subtree
  :ensure t
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)
        ("<backtab>" . dired-subtree-cycle)))

(use-package ls-lisp
  :config 
  (setq ls-lisp-use-insert-directory-program nil)
  (setq ls-lisp-dirs-first t)
  (setq ls-lisp-use-localized-time-format t))


;;; ---------------------------------------------------------------------------
;;; Completion
;;; ---------------------------------------------------------------------------

(use-package flx
  :ensure t)

(use-package ivy
  :ensure t
  :init
  (require 'flx)
  (setq ivy-wrap t)
  (setq ivy-use-virtual-buffers nil)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-height 15)
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  :config
  (ivy-mode 1)
  :bind
  (("C-c C-r" . ivy-resume)))

(use-package ivy-rich
  :ensure t
  :init
  (setq ivy-rich-abbreviate-paths t)
  (setq ivy-virtual-abbreviate 'full)
  (setq ivy-rich-switch-buffer-align-virtual-buffer t)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

(use-package counsel
  :ensure t
  :init
  (setq confirm-nonexistent-file-or-buffer t)
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)))

;; Used for better sorting during `counsel-M-x`
(use-package smex
  :ensure t)


;;; ---------------------------------------------------------------------------
;;; In-buffer completion
;;; ---------------------------------------------------------------------------

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
  :hook (prog-mode . company-mode)
  :config
  (defun company-mode/backend-with-yas (backend)
    (if (and (listp backend) (member 'company-yasnippet backend))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

(use-package company-web
  :ensure t)


;;; ---------------------------------------------------------------------------
;;; Miscellaneous
;;; ---------------------------------------------------------------------------

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package dash-at-point
  :ensure t
  :bind
  (("C-c d" . dash-at-point)))


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


;;; Programming modes

(use-package magit
  :ensure t
  :bind
  (("C-c g" . magit-status)))



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
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)))

(use-package web-mode
  :ensure t
  :mode "\\.html\\'"
  :hook
  (web-mode . (lambda () 
                (add-to-list (make-local-variable 'company-backends) 'company-web-html))))

(use-package js2-mode
  :ensure t
  :defer t)

(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :hook (prog-mode . aggressive-indent-mode))


;;; ---------------------------------------------------------------------------
;;; Applications
;;; ---------------------------------------------------------------------------

;;; Elfeed

(use-package elfeed
  :ensure t)

(use-package elfeed-org
  :after elfeed
  :ensure t
  :init
  (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))
  :config
  (elfeed-org))


;;; ---------------------------------------------------------------------------
;;; org-mode
;;; ---------------------------------------------------------------------------

(use-package org
  :ensure t
  :bind
  (("C-c c" . org-capture)
   ("C-c a" . org-agenda))
  :config
  ;; Files & Directories
  (setq org-directory (expand-file-name "~/org"))
  (setq org-agenda-files '("~/org"))
  (setq org-default-notes-file (concat org-directory "/capture.org"))

  ;; UI
  (setq org-fontify-whole-heading-line t)
  (setq org-hide-emphasis-markers t)
  (setq org-hide-leading-stars t)
  (setq org-odd-levels-only nil)
  (setq org-time-stamp-custom-formats '("<%e %b, %Y>" .
                                        "<%e %b, %Y %H:%M>"))

  ;; Behaviour
  (setq org-log-done t)
  (setq org-use-fast-todo-selection t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w)" "HOLD(h)" "|" "CANCELLED(c)"))
        org-todo-repeat-to-state "NEXT")

  (setq org-todo-keyword-faces
        '(("NEXT" :inherit font-lock-function-name-face :weight bold)
          ("WAITING" :inherit warning)))

  (setq org-special-ctrl-a/e t
        org-special-ctrl-k t)
  (setq org-yank-adjusted-subtrees t)
  
  ;; Agenda
  
  ;; Capture
  (setq org-capture-templates
        '(("t" "Todo" entry (file "~/org/capture.org")
           "* TODO %?")
          ("n" "Note" entry (file"~/org/capture.org")
           "* %?"))))

(use-package org-super-agenda
  :ensure t)

(use-package org-brain
  :ensure t
  :bind
  (("C-c v" . org-brain-visualize)))


;;; ---------------------------------------------------------------------------
;;; eshell
;;; ---------------------------------------------------------------------------

(use-package shrink-path
  :ensure t)

(use-package eshell
  :preface
  (defun om/current-git-branch ()
    (let ((branch (car (loop for match in (split-string (shell-command-to-string "git branch") "\n")
                             when (string-match "^\*" match)
                             collect match))))
      (if (not (eq branch nil))
          (concat " [" (substring branch 2) "]")
        "")))

  (defun om/eshell-prompt ()
    (concat
     "\["
     (propertize
      (concat
       (user-login-name)
       "@"
       (when (string-match "^[^.]+" (system-name))
         (match-string 0 (system-name))))
      'face `(:inherit 'font-lock-string-face)) 
     "\]:" 
     (let ((path (shrink-path-prompt default-directory)))
       (concat (propertize (car path) 'face 'font-lock-comment-face)
               (propertize (cdr path) 'face 'font-lock-constant-face)
               (propertize (om/current-git-branch)
                           'face 'font-lock-function-name-face)
               (propertize (if (= (user-uid) 0) " #" " $") 'face 'eshell-prompt-face)
               (propertize " " 'face 'default)))))

  (defun om/eshell-here () 
    (interactive)
    (let* ((parent (if (buffer-file-name)
                       (file-name-directory (buffer-file-name))
                     default-directory))
           (height (/ (window-total-height) 3))
           (name   (car (last (split-string parent "/" t)))))
      (split-window-vertically (- height))
      (other-window 1)
      (eshell "new")
      (rename-buffer (concat "*eshell: " name "*"))

      (insert (concat "ls"))
      (eshell-send-input)))

  (defun om/delete-single-window (&optional window)
    (interactive)
    (save-current-buffer
      (setq window (or window (selected-window)))
      (select-window window)
      (kill-buffer)
      (if (one-window-p t)
          (delete-frame)
        (delete-window (selected-window)))))

  (defun eshell/x ()
    (om/delete-single-window))
  
  (defun eshell/clear ()
    "Clear the eshell buffer."
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))
  :init
  ;; customize prompt
  (setq eshell-prompt-function 'om/eshell-prompt)
  (setq eshell-prompt-regexp "^.*[#$] ")
  ;; others
  (setq eshell-scroll-to-bottom-on-input 'all)
  (setq eshell-error-if-no-glob t)
  (setq eshell-hist-ignoredups t)
  (setq eshell-save-history-on-exit t)
  (setq eshell-destroy-buffer-when-process-dies t)
  :bind
  (("C-c e" . om/eshell-here)))


;;;
;;; Variables set by Emacs customization facility
;;; 

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe-mode (quote (4 . 4)) nil (fringe))
 '(package-selected-packages
   (quote
    (org-bullets elfeed-org company-yasnippets company-yasnippet yasnippet-snippets dired-subtree smex iedit projectile counsel-notmuch notmuch ivy-rich git-gutter-fringe dired-collapse-mode dired-k flx all-the-icons-ivy ivy-hydra counsel ivy dired-collapse spaceline-all-the-icons kubernetes terraform-mode markdown-mode helm-org-rifle org-brain nlinum-hl solaire-mode elpy eldoc-eval nlinum doom-themes elfeed beacon helm-ag helm-dash helm-mode-manager glsl-mode ace-jump-zap ace-window avy ace-jump-mode dash-at-point move-text groovy-mode gradle-mode yaml-mode helm-descbinds dired+ page-break-lines fill-column-indicator helm-company neotree company-web company-restclient ob-restclient restclient anzu js2-mode json-mode web-mode use-package spaceline zenburn-theme yasnippet window-numbering which-key undo-tree slime-company popup paredit multiple-cursors magit helm-projectile expand-region aggressive-indent)))
 '(safe-local-variable-values (quote ((bug-reference-bug-regexp . "#\\(?2:[0-9]+\\)"))))
 '(spaceline-all-the-icons-eyebrowse-display-name nil)
 '(spaceline-all-the-icons-hide-long-buffer-path nil)
 '(spaceline-all-the-icons-highlight-file-name t)
 '(spaceline-all-the-icons-icon-set-window-numbering (quote circle))
 '(spaceline-all-the-icons-window-number-always-visible nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#21242b" :foreground "#bbc2cf" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "nil" :family "Menlo"))))
 '(dired-directory ((t (:foreground "#51afef"))))
 '(mode-line ((t (:background "#282c34" :box (:line-width 4 :color "#282c34")))))
 '(mode-line-inactive ((t (:background "#1d2026" :foreground "#545668" :box (:line-width 4 :color "#1d2026")))))
 '(solaire-default-face ((t (:inherit default :background "#282c34"))))
 '(solaire-mode-line-face ((t (:inherit mode-line :background "#1c1f25" :box (:line-width 4 :color "#1c1f25")))))
 '(solaire-mode-line-inactive-face ((t (:inherit mode-line-inactive :background "#21242b" :box (:line-width 4 :color "#21242b")))))
 '(spaceline-highlight-face ((t (:inherit (quote mode-line)))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "gray")))))

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
