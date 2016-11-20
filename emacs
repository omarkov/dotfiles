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

(when (eq window-system 'mac)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'option)
  (setq mac-function-modifier 'hyper))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil)

;;;
;;; Setup default modes
;;;

;; Disable scrollbars, the toolbar and blinking cursor
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (blink-cursor-mode -1))

(delete-selection-mode 1)
(show-paren-mode 1)
(column-number-mode 1)
(global-visual-line-mode 1)
(global-hl-line-mode 1)
(winner-mode 1)

(add-hook 'prog-mode-hook 'linum-mode)
(setq linum-format "%4d ")


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

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn 'no-confirm))

(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-helm-mode))

(use-package powerline
  :ensure t
  :after spaceline-config
  :init
  (setq powerline-default-separator 'wave)
  :config
  (powerline-default-theme))

(use-package neotree
  :ensure t
  :bind
  (("C-c n" . neotree)))

;;; Global modes

(use-package diminish
  :ensure t
  :config
  (diminish visual-line-mode))

(use-package helm
  :ensure t
  :diminish helm-mode
  :init 
  (require 'helm-config)
  (setq helm-split-window-in-side-p t)
  (setq helm-ff-skip-boring-files t)
  (setq helm-boring-file-regexp-list
	'("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "~$"
	  "\\.so$" "\\.a$" "\\.elc$" "\\.fas$" "\\.fasl$" "\\.pyc$" "\\.pyo$"))
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
  (:map company-active-map
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

(use-package move-text
  :ensure t
  :bind
  (("M-n" . move-text-down)
   ("M-p" . move-text-up)))

(use-package fill-column-indicator
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'fci-mode))


;;; Programming modes

;(load (expand-file-name "~/quicklisp/slime-helper.el"))

(use-package slime 
  :config
  (require 'slime-company) 
  (setq slime-net-coding-system 'utf-8-unix)
  (setq slime-lisp-implementations
	'((ccl64 ("/usr/local/bin/ccl64" "-K utf-8") :coding-system utf-8-unix)
	  (sbcl ("/usr/local/bin/sbcl") :coding-system utf-8-unix))))

(use-package paredit
  :ensure t
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


;;;
;;; Variables set by Emacs customization facility
;;; 

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(neo-auto-indent-point t)
 '(neo-hidden-regexp-list
   (quote
    ("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "\\.fasl")))
 '(neo-theme (quote nerd))
 '(neo-window-fixed-size nil)
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
    (move-text groovy-mode gradle-mode yaml-mode helm-descbinds dired+ sr-speedbar page-break-lines fill-column-indicator helm-company hydra neotree company-web company-restclient ob-restclient restclient anzu js2-mode json-mode web-mode use-package spaceline sx zenburn-theme yasnippet window-numbering which-key undo-tree slime-company popup paredit multiple-cursors magit helm-projectile expand-region aggressive-indent)))
 '(safe-local-variable-values (quote ((Package . CCL)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:background "#3F3F3F" :foreground "#DCDCCC"))))
 '(helm-source-header ((t (:background "#2B2B2B" :foreground "#F0DFAF" :box nil :underline nil :weight bold))))
 '(linum ((t (:background "#3F3F3F" :foreground "#7f7f7f" :height 0.8))))
 '(mode-line ((t (:background "#2B2B2B" :foreground "#8FB28F" :box nil))))
 '(mode-line-highlight ((t (:box nil))))
 '(mode-line-inactive ((t (:background "#383838" :foreground "#5F7F5F" :box nil))))
 '(neo-dir-link-face ((t (:inherit font-lock-function-name-face))))
 '(neo-file-link-face ((t nil))))


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
