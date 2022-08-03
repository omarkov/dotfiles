;;; emacs --- Initialization file for Emacs -*- Mode: emacs-lisp -*-


;;; ---------------------------------------------------------------------------
;;; Configure environment settings
;;; ---------------------------------------------------------------------------

;;; Allow loading emacs lisp code from the emacs directory
(add-to-list 'load-path "~/.emacs.d/lisp")


;;; MacOS
;;;
;;; - Use command key as meta

(when (eq window-system 'mac)
  ;(set-default-font "Menlo")
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'option)
  (setenv "LANG" "en_US.UTF-8"))

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

(setq next-line-add-newlines t)

;;;
;;; Setup default modes
;;;

(delete-selection-mode 1)
(column-number-mode 1)


(setq delete-old-versions t
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))


;;;
;;; Setup some global keybindings
;;; 

(global-set-key (kbd "\C-x k") 'kill-this-buffer)
(global-set-key [escape] 'keyboard-escape-quit)
(define-key isearch-mode-map [escape] 'isearch-cancel)


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

(require 'use-package-ensure)
(setq use-package-always-ensure t)


;;; ---------------------------------------------------------------------------
;;; Startup settings
;;;
;;; Disable any startup messages and keep the scratch buffer empty.
;;; Setup a nice dashboard.
;;; ---------------------------------------------------------------------------

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))


;;; ---------------------------------------------------------------------------
;;; UI Setup
;;;
;;; Customize visual settings.  Loads the Doom One theme with solaire mode.
;;; Also includes the mode-line from Doom Emacs.
;;; ---------------------------------------------------------------------------

(winner-mode 1)
(windmove-default-keybindings)

;; disable blinking cursor
(when (display-graphic-p)
  (blink-cursor-mode -1))

;;; built-in in Emacs 29
;; (use-package pixel-scroll
;;   :ensure nil
;;   :config
;;   (pixel-scroll-mode 1)
;;   (setq fast-but-imprecise-scrolling t)
;;   (setq pixel-resolution-fine-flag t))

(setq echo-keystrokes 0.1)

(defalias 'yes-or-no-p 'y-or-n-p)

;;; enable some better defaults
(use-package better-defaults
  :config
  ;; fix fullscreen (disabled on mac otherwise)
  (when (eq window-system 'mac)
    (menu-bar-mode 1)))

;;; 
;;; Configure theme
;;; 

(use-package doom-themes
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
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  ;; FIXME: move current face customizations here and make them theme
  ;; specific.  Should probably use something like
  ;; (doom-themes-set-faces 'doom-one '(face :background "gray"))
  (load-theme 'doom-one 'no-confirm))

;; brighten buffers (that represent real files)
(use-package solaire-mode
  :requires doom-themes
  :config
  (solaire-global-mode t))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

;;;
;;; Configure additional UI packages
;;; 

;;; Display a file tree (bound to C-c n)
(use-package neotree
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
  :diminish anzu-mode
  :init
  (setq anzu-cons-mode-line-p nil) ; required to work with doom-modeline
  :config
  (global-anzu-mode))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package beacon
  :config
  (beacon-mode))

(use-package diminish
  :diminish visual-line-mode
  :diminish eldoc-mode
  :diminish auto-revert-mode)

(use-package shackle
  :ensure t
  :config
  (setq shackle-rules '(("\\`\\*WoMan .*\\*\\'" :regexp t :select t :popup t :align right :size 0.5)
                        ("\\`\\*vterm.*\\*\\'" :regexp t :select t :popup t :align bottom :size 0.3)))
  (shackle-mode 1))

;;; ---------------------------------------------------------------------------
;;; Dired
;;; ---------------------------------------------------------------------------

(use-package dired
  :ensure nil
  :init
  (setq dired-listing-switches "-alh"))

;;; Use dired-x for its omit-mode
(use-package dired-x
  :ensure nil
  :init
  (setq-default dired-omit-files-p t)
  (setq dired-omit-verbose nil)
  (setq dired-omit-files "^\\...+$")
  :bind
  (:map dired-mode-map
        (")" . dired-omit-mode)))

(use-package dired-collapse
  :hook ((dired-mode . dired-collapse-mode)
         (dired-mode . (lambda () (toggle-truncate-lines 1)))))

;; (use-package dired-k
;;   :hook ((dired-initial-position . dired-k)
;;          (dired-after-readin . dired-k-no-revert))
;;   :init
;;   (setq dired-k-style 'git)
;;   (setq dired-k-human-readable t))

(use-package dired-subtree
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)
        ("<backtab>" . dired-subtree-cycle)))

(use-package ls-lisp
  :ensure nil
  :config
  (setq ls-lisp-use-insert-directory-program nil)
  (setq ls-lisp-dirs-first t)
  (setq ls-lisp-use-localized-time-format t))

(use-package autorevert
  :ensure nil
  :init
  (global-auto-revert-mode 1)
  :config
  ;; Also auto refresh dired, but be quiet about it
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil))


;;; ---------------------------------------------------------------------------
;;; TRAMP
;;; ---------------------------------------------------------------------------

(use-package tramp
  :ensure nil
  :config
  (add-to-list 'tramp-connection-properties '("/ssh:" "direct-async-process" t))
  (setq tramp-allow-unsafe-temporary-files t)
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))

;;; ---------------------------------------------------------------------------
;;; Completion
;;; ---------------------------------------------------------------------------

(use-package flx)

(use-package ivy
  :init
  (require 'flx)
  (setq ivy-wrap t)
  (setq ivy-use-virtual-buffers nil)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-height 15)
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  :config
  (ivy-mode 1)
  :bind
  (("C-c C-r" . ivy-resume)))

(use-package ivy-rich
  :config
  (ivy-rich-mode 1)
  :init
  (setq ivy-format-function #'ivy-format-function-line)
  (setq ivy-rich-parse-remote-buffer nil)
  (setq ivy-rich-parse-remote-file-path nil)
  (setq ivy-virtual-abbreviate 'full))

(use-package counsel
  :init
  (setq confirm-nonexistent-file-or-buffer t)
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)))

;; Used for better sorting during `counsel-M-x`
(use-package smex)


;;; ---------------------------------------------------------------------------
;;; In-buffer completion
;;; ---------------------------------------------------------------------------

(use-package company
  :diminish company-mode
  :preface
  (defun company-mode/backend-with-yas (backend)
    (if (and (listp backend) (member 'company-yasnippet backend))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
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
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

(use-package company-web)


;;; ---------------------------------------------------------------------------
;;; Miscellaneous
;;; ---------------------------------------------------------------------------

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(use-package dash-at-point
  :bind
  (("C-c d" . dash-at-point)))


(use-package ace-jump-mode
  :bind
  (("C-c SPC" . ace-jump-mode)))

(use-package ace-jump-zap
  :bind
  (("C-c z" . ace-jump-zap-up-to-char)))

(use-package ace-window
  :bind
  (("M-p" . ace-window)))


(use-package mwim
  :bind
  (("C-a" . mwim-beginning)
   ("C-e" . mwim-end)))


;;; Programming modes

(use-package magit
  :bind
  (("C-c g" . magit-status)))


(use-package slime
  :config
  (setq slime-net-coding-system 'utf-8-unix)
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy slime-listener-hooks slime-indentation slime-company))

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
  :diminish paredit-mode
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)))

(use-package web-mode
  :mode "\\.html\\'"
  :hook
  (web-mode . (lambda ()
                (add-to-list (make-local-variable 'company-backends) 'company-web-html))))

(use-package js2-mode
  :defer t)

(use-package rjsx-mode
  :ensure t)

(use-package tide
  :ensure t
  :after (rjsx-mode company flycheck)
  :hook ((rjsx-mode . tide-setup)
         (rjsx-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))


;;; ---------------------------------------------------------------------------
;;; Applications
;;; ---------------------------------------------------------------------------



;;; ---------------------------------------------------------------------------
;;; org-mode
;;; ---------------------------------------------------------------------------

(use-package org
  :bind
  (("C-c c" . org-capture)
   ("C-c a" . org-agenda))
  :config
  ;; Files & Directories
  (setq org-directory (expand-file-name "~/org"))
  ;(setq org-agenda-files '("~/org/inbox.org" "~/org/gtd.org" "~/org/tickler.org"))
  (setq org-default-notes-file "~/org/inbox.org")

  ;; UI
  (setq org-fontify-whole-heading-line t)
  (setq org-hide-emphasis-markers t)
  (setq org-hide-leading-stars t)
  (setq org-odd-levels-only nil)
  (setq org-time-stamp-custom-formats '("<%e %b, %Y>" .
                                        "<%e %b, %Y %H:%M>"))

  ;; Behaviour
  (setq org-log-done nil)
  (setq org-use-fast-todo-selection t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w)" "HOLD(h)" "|" "CANCELLED(c)"))
        org-todo-repeat-to-state "NEXT")

  (setq org-todo-keyword-faces
        '(("NEXT" :inherit font-lock-function-name-face :weight bold)
          ("WAITING" :inherit warning)))

  (setq org-return-follows-link t)
  (setq org-special-ctrl-a/e t
        org-special-ctrl-k t)
  (setq org-yank-adjusted-subtrees t)
  
  ;; Agenda
  
  ;; Capture
  (setq org-capture-templates
        '(("t" "Create a new TODO entry in the inbox." entry
           (file+headline "~/org/inbox.org" "Tasks")
           "* TODO %i%?")
          ("T" "Create a new tickler entry." entry
           (file+headline "~/org/tickler.org" "Tickler")
           "* %i%? \n %U")
          ("n" "Create a new timestamped note." plain
           (file (lambda () (concat "~/pkm/Notes/" (format-time-string "%F-%H%M.org"))))
           "#+TITLE: %?"))))


;;; ---------------------------------------------------------------------------
;;; vterm
;;; ---------------------------------------------------------------------------

(use-package vterm
  :ensure t
  :config
  (setq vterm-kill-buffer-on-exit t)
  (add-to-list 'vterm-eval-cmds '("woman-find-file" woman-find-file)))


;;; ---------------------------------------------------------------------------
;;; eshell
;;; ---------------------------------------------------------------------------

(use-package shrink-path)

(use-package eshell
  :ensure nil
  :preface
  (defun om/current-git-branch ()
    (let ((branch (car (cl-loop for match in (split-string (shell-command-to-string "git branch") "\n")
                                when (string-match "^\*" match)
                                collect match))))
      (if (not (eq branch nil))
          (concat " [" (substring branch 2) "]")
        "")))

  (defun om/eshell-prompt ()
    (concat
     "\["
     (propertize (concat  (user-login-name)
                          "@"
                          (when (string-match "^[^.]+" (system-name))
                            (match-string 0 (system-name))))
                 'face 'font-lock-string-face)
     "\]:"
     (let ((path (shrink-path-prompt default-directory)))
       (concat (propertize (car path) 'face 'font-lock-comment-face)
               (propertize (cdr path) 'face 'font-lock-comment-face)
               (propertize (om/current-git-branch)
                           'face 'font-lock-function-name-face)
               (propertize (if (= (user-uid) 0) "#" "$") 'face 'default)
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

  (defun eshell/x (&rest args)
    (kill-buffer-and-window))
  
  (defun eshell/clear ()
    "Clear the eshell buffer."
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))
  :config
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
 '(company-box-icons-alist (quote company-box-icons-all-the-icons))
 '(custom-safe-themes
   (quote
    ("2d1fe7c9007a5b76cea4395b0fc664d0c1cfd34bb4f1860300347cdad67fb2f9" "728eda145ad16686d4bbb8e50d540563573592013b10c3e2defc493f390f7d83" "aa0a998c0aa672156f19a1e1a3fb212cdc10338fb50063332a0df1646eb5dfea" "4597d1e9bbf1db2c11d7cf9a70204fa42ffc603a2ba5d80c504ca07b3e903770" default)))
 '(doom-modeline-mode t)
 '(doom-one-padded-modeline t)
 '(fringe-mode (quote (4 . 4)) nil (fringe))
 '(package-selected-packages
   (quote
    (rjsx-mode tide ivy-posframe better-defaults request deadgrep deft company-box company-posframe go-mode php-mode discover pcomplete-extension flycheck esh-autosuggest dashboard mwim doom-modeline ttl-mode docker docker-compose-mode docker-tramp dockerfile-mode shrink-path org-bullets elfeed-org company-yasnippets company-yasnippet yasnippet-snippets dired-subtree smex iedit projectile counsel-notmuch notmuch ivy-rich git-gutter-fringe dired-collapse-mode dired-k flx all-the-icons-ivy ivy-hydra counsel ivy dired-collapse spaceline-all-the-icons kubernetes terraform-mode markdown-mode helm-org-rifle org-brain nlinum-hl solaire-mode elpy eldoc-eval nlinum doom-themes elfeed beacon helm-ag helm-dash helm-mode-manager glsl-mode ace-jump-zap ace-window avy ace-jump-mode dash-at-point move-text groovy-mode gradle-mode yaml-mode helm-descbinds dired+ page-break-lines fill-column-indicator helm-company neotree company-web company-restclient ob-restclient restclient anzu js2-mode json-mode web-mode use-package spaceline zenburn-theme yasnippet window-numbering which-key undo-tree slime-company popup paredit multiple-cursors magit helm-projectile expand-region aggressive-indent))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :foreground "#bbc2cf" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "nil" :family "Menlo"))))
 ;; '(default ((t (:inherit nil :stipple nil :background "#21242b" :foreground "#bbc2cf" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "nil" :family "Menlo"))))
 '(dired-directory ((t (:foreground "#51afef"))))
 '(eshell-prompt ((t (:inherit default :foreground "#bbc2cf" :weight normal))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "gray")))))

;;;
;;; Custom functions
;;;

;;; auto-indent after yank
(defadvice insert-for-yank-1 (after indent-region activate)
  "Indent yanked region in certain modes, prefix to disable."
  (if (and (not current-prefix-arg)
           (member major-mode '(sh-mode
                                emacs-lisp-mode lisp-mode
                                c-mode c++-mode objc-mode d-mode java-mode cuda-mode
                                LaTeX-mode TeX-mode
                                xml-mode html-mode css-mode)))
      (indent-region (region-beginning) (region-end) nil)))

(provide 'emacs)

;;; emacs ends here
