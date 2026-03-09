;;; emacs --- Initialization file for Emacs -*- Mode: emacs-lisp -*-


;;; ---------------------------------------------------------------------------
;;; Configure environment settings
;;; ---------------------------------------------------------------------------

;;; Allow loading emacs lisp code from the emacs directory
(add-to-list 'load-path "~/.emacs.d/lisp")

;;; Put all auto-generated configurations in a separate file
;(setq custom-file (locate-user-emacs-file "custom.el"))
;(load custom-file :no-error-if-file-is-missing)


;;; MacOS
(when (or (eq window-system 'ns) (eq window-system 'mac))
  ;(set-default-font "Menlo")
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'option)
  (setenv "LANG" "en_US.UTF-8")
  (setenv "PATH" "/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin::/usr/local/MacGPG2/bin:/usr/local/bin:/opt/homebrew/bin:/Users/entrox/.local/bin")
  (setq exec-path (split-string (getenv "PATH") path-separator)))

;;; Windows
(when (eq window-system 'w32)
  (set-face-attribute 'default nil :font "Consolas 11")
  (setq-default default-directory (file-name-as-directory (getenv "HOMEPATH")))

  ;; Setup proxy on my work machine
  (when (string-match-p "CMTC*" (system-name))
    (setq url-proxy-services
          '(("no_proxy" . "^\\(localhost\\|10.*\\)")
            ("http" . "localhost:3128")
            ("https" . "localhost:3128")))))


;;; ---------------------------------------------------------------------------
;;; General editing behaviour
;;; ---------------------------------------------------------------------------

;;;
;;; Setup default modes
;;;

(delete-selection-mode 1)
(column-number-mode 1)

(setq delete-old-versions t
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(recentf-mode 1)

;;;
;;; Setup some global keybindings
;;; 

(defun om/keyboard-quit-dwim ()
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

;(global-set-key (kbd "\C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x k") (lambda ()
                              (interactive)
                              (kill-buffer (current-buffer))))
(global-set-key [escape] 'keyboard-escape-quit)
(define-key isearch-mode-map [escape] 'isearch-cancel)
(define-key global-map (kbd "C-g") #'om/keyboard-quit-dwim)


;;; ---------------------------------------------------------------------------
;;; Setup the packaging system
;;;
;;; Add MELPA to the list of package repositories, initialize the
;;; system and make sure that USE-PACKAGE is installed.  All
;;; subsequent package definitions make use of it.
;;; ---------------------------------------------------------------------------


;(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;(package-initialize)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Hide the byte-compilation warnings when loading packages
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))



;;; ---------------------------------------------------------------------------
;;; Startup settings
;;;
;;; Disable any startup messages and keep the scratch buffer empty.
;;; ---------------------------------------------------------------------------

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)


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

(setq echo-keystrokes 0.1)

(defalias 'yes-or-no-p 'y-or-n-p)

;;; enable some better defaults
(use-package better-defaults)

(setq treesit-font-lock-level 3)

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
  (setq doom-themes-treemacs-enable-variable-pitch nil)
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (doom-themes-treemacs-config)
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
  :custom
  (doom-one-padded-modeline t)
  :hook (after-init . doom-modeline-mode))

(use-package treemacs
  :config
  (treemacs-follow-mode t)
  (treemacs-hide-gitignored-files-mode t))

;;;
;;; Configure additional UI packages
;;; 

(use-package dashboard
  :init
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((recents . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (registers . 5)))
  (dashboard-setup-startup-hook))

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
                        ("\\`\\*vterm.*\\*\\'" :regexp t :select t :popup t :align bottom :size 0.33)
                        ("\\`\\*claude.*\\*\\'" :regexp t :select t :popup t :align right :size 0.5)))
  (shackle-mode 1))

;;; ---------------------------------------------------------------------------
;;; Dired
;;; ---------------------------------------------------------------------------

(use-package dired
  :ensure nil
  :hook
  ((dired-mode . hl-line-mode))
  :config
  (setq dired-listing-switches "-alh")
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

;;; Use dired-x for its omit-mode
(use-package dired-x
  :ensure nil
  :init
  (setq-default dired-omit-files-p t)
  (setq dired-omit-verbose nil)
  (setq dired-omit-files "^\\...+$")
  :hook ((dired-mode . dired-omit-mode))
  :bind
  (:map dired-mode-map
        (")" . dired-omit-mode)))

(use-package dired-collapse
  :hook ((dired-mode . dired-collapse-mode)
         (dired-mode . (lambda () (toggle-truncate-lines 1)))))

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
;;; Browse URL
;;; ---------------------------------------------------------------------------

(use-package browse-url
  :config
  (setq browse-url-handlers
        ;; open PDF links in external viewer (Preview.app)
        '(("\\.pdf\\'" . (lambda (url &rest _args)
                           (let ((file (url-unhex-string
                                        (string-remove-prefix "file://" url))))
                             (start-process "open-pdf" nil "open" "-a" "Preview" file)))))))

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

(use-package vertico
  :init
  (vertico-mode)

  :config
  (setq vertico-cycle t))

(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :after vertico
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode)

  :config
  (setq marginalia-align 'right))

(use-package consult
  :bind
  (("C-x b" . consult-buffer)
   ("M-y" . consult-yank-pop))
  :config
  (consult-customize
   consult-buffer
   :preview-key '("M-P")))



;;; ---------------------------------------------------------------------------
;;; In-buffer completion
;;; ---------------------------------------------------------------------------

(use-package corfu
  :init
  (global-corfu-mode)

  :config
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  
  ;; Enable indentation+completion using the TAB key.
  (setq tab-always-indent 'complete))


;;; ---------------------------------------------------------------------------
;;; Miscellaneous
;;; ---------------------------------------------------------------------------

(use-package dash-at-point
  :bind
  (("C-c d" . dash-at-point)))


(use-package ace-window
  :bind
  (("M-o" . ace-window)))

(use-package avy
  :bind
  (("C-." . avy-goto-char-timer)))

(use-package mwim
  :bind
  (("C-a" . mwim-beginning)
   ("C-e" . mwim-end)))



;;; ---------------------------------------------------------------------------
;;; Tree-sitter & Eglot setup
;;; ---------------------------------------------------------------------------

(use-package treesit
  :config
  (setq treesit-language-source-alist
        '((python . ("https://github.com/tree-sitter/tree-sitter-python" "master" "src"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))))
  
  ;; Install grammars if not present
  (dolist (lang '(python typescript tsx))
    (unless (treesit-language-available-p lang)
      (treesit-install-language-grammar lang))))


;;; Eglot configuration
(use-package eglot
  :config
  ;; TypeScript language server
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode tsx-ts-mode) . ("typescript-language-server" "--stdio")))
  
  ;; Optional: configure eglot behavior
  (setq eglot-autoshutdown t)
  (setq eglot-sync-connect nil))


;;; ---------------------------------------------------------------------------
;;; Programming modes
;;; ---------------------------------------------------------------------------

(use-package magit
  :bind
  (("C-c g" . magit-status)))


(use-package sly
  :config
  (setq inferior-lisp-program "sbcl --dynamic-space-size 4096")
  (setq sly-net-coding-system 'utf-8-unix))


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

;;; TypeScript
(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :hook ((typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure))
  :config
  (setq typescript-ts-mode-indent-offset 2))

;;; Python
(use-package python
  :mode (("\\.py\\'" . python-ts-mode))
  :hook (python-ts-mode . eglot-ensure))

(put 'flycheck-python-pylint-executable 'safe-local-variable 'stringp)
(put 'flycheck-python-flake8-executable 'safe-local-variable 'stringp)
(put 'flycheck-python-mypy-executable 'safe-local-variable 'stringp)



;;; ---------------------------------------------------------------------------
;;; Applications
;;; ---------------------------------------------------------------------------

(use-package auth-source)


;;; ---------------------------------------------------------------------------
;;; Denote
;;; ---------------------------------------------------------------------------

(use-package denote
  :config
  (setq denote-known-keywords '("emacs" "devops" "admin" "management" "tech"))
  
  :bind
  (("C-c n n" . denote)
   ("C-c n f" . denote-open-or-create)
   ("C-c n r" . denote-rename-file)
   ("C-c n k" . denote-rename-file-keywords)
   ("C-c n l" . denote-link)))

;;; ---------------------------------------------------------------------------
;;; IRC
;;; ---------------------------------------------------------------------------

(use-package erc
  :preface
  (defun znc ()
    (interactive)
    (erc-tls :server "irc.entrox.org"
             :port 6697
             :client-certificate t))

  :custom
  (erc-fill-column 100)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 22)
  
  (erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "324" "353" "329" "333 332"))
  (erc-lurker-threshold-time 43200)
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
			     "324" ; modes https://www.alien.net.au/irc/irc2numerics.html
			     "329" ; channel creation date
			     "332" ; topic notice
			     "333" ; who set the topic
			     "353" ; names notice
			     )))


;;; ---------------------------------------------------------------------------
;;; LLM
;;; ---------------------------------------------------------------------------

;;; Agent Shell

(use-package agent-shell
  :ensure t
  :ensure-system-package
  ((claude . "brew install claude-code")
   (claude-agent-acp . "npm install -g @zed-industries/claude-agent-acp"))
  :config
  (setq agent-shell-preferred-agent-config (agent-shell-anthropic-make-claude-code-config)))

;;; direnv handling

(use-package inheritenv
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))

(use-package envrc
  :ensure t
  :config
  (envrc-global-mode))


;;; ---------------------------------------------------------------------------
;;; org-mode
;;; ---------------------------------------------------------------------------

(use-package org
  :config
  ;; UI
  (setq org-fontify-whole-heading-line t)
  (setq org-hide-emphasis-markers t)
  (setq org-hide-leading-stars nil)
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

  ;; Babel
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (shell . t))))


;;; ---------------------------------------------------------------------------
;;; vterm
;;; ---------------------------------------------------------------------------

(use-package vterm
  :ensure t
  :config
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 10000)
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

;;; ---------------------------------------------------------------------------
;;; Markdown Mode
;;; ---------------------------------------------------------------------------

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :custom
  (markdown-command "pandoc -f gfm -t html5"))


;;; ---------------------------------------------------------------------------
;;; Customize section
;;; ---------------------------------------------------------------------------

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
    ("2d1fe7c9007a5b76cea4395b0fc664d0c1cfd34bb4f1860300347cdad67fb2f9" "728eda145ad16686d4bbb8e50d540563573592013b10c3e2defc493f390f7d83" "aa0a998c0aa672156f19a1e1a3fb212cdc10338fb50063332a0df1646eb5dfea" "4597d1e9bbf1db2c11d7cf9a70204fa42ffc603a2ba5d80c504ca07b3e903770" default)))
 '(fringe-mode (quote (4 . 4)) nil (fringe))
 '(package-selected-packages
   (quote
    (rjsx-mode tide ivy-posframe better-defaults request deadgrep deft company-box company-posframe go-mode php-mode discover pcomplete-extension flycheck esh-autosuggest dashboard mwim doom-modeline ttl-mode docker docker-compose-mode docker-tramp dockerfile-mode shrink-path org-bullets elfeed-org company-yasnippets company-yasnippet yasnippet-snippets dired-subtree smex iedit projectile counsel-notmuch notmuch ivy-rich git-gutter-fringe dired-collapse-mode dired-k flx all-the-icons-ivy ivy-hydra counsel ivy dired-collapse spaceline-all-the-icons kubernetes terraform-mode markdown-mode nlinum-hl solaire-mode eldoc-eval nlinum doom-themes elfeed beacon glsl-mode ace-jump-zap ace-window avy ace-jump-mode dash-at-point move-text groovy-mode gradle-mode yaml-mode dired+ page-break-lines fill-column-indicator company-web company-restclient ob-restclient restclient anzu js2-mode json-mode web-mode use-package spaceline zenburn-theme yasnippet window-numbering which-key undo-tree slime-company popup paredit multiple-cursors magit expand-region aggressive-indent))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :foreground "#bbc2cf" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "nil" :family "Menlo"))))
 '(dired-directory ((t (:foreground "#51afef"))))
 '(eshell-prompt ((t (:inherit default :foreground "#bbc2cf" :weight normal)))))


(provide 'emacs)

;;; emacs ends here
