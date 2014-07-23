;;; Personal Customizations

(mapc (lambda(mode) (when (fboundp mode) (apply mode '(-1))))
      '(tool-bar-mode
        menu-bar-mode
        scroll-bar-mode))

(setq ring-bell-function #'ignore
      inhibit-startup-screen t
      indent-tabs-mode nil)

(icomplete-mode t)

;; Give buffers better names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-after-kill-buffer-p t)

;; Smoother scrolling
(setq mouse-wheel-scroll-amount '(4 ((shift) . 4)) ; four lines at a time
      mouse-wheel-progressive-speed nil ; don't accelerate scrolling
      mouse-wheel-follow-mouse 't ; scroll window under mouse
      scroll-step 1 ; keyboard scroll one line at a time
      scroll-conservatively 10000 ; don't jump around as much
      auto-window-vscroll nil) ; magic?

;;; Package Support

(mapc (lambda (p) (push p load-path))
      '("~/.emacs.d/use-package/"))

(require 'use-package)
(font-lock-add-keywords 'emacs-lisp-mode use-package-font-lock-keywords)
(font-lock-add-keywords 'lisp-interaction-mode use-package-font-lock-keywords)
(require 'package)

(defmacro depends (name &rest body)
  (declare (indent defun))
  `(eval-after-load ,name (lambda () ,@body)))

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(when (and (member "--" command-line-args)
	   (member "-refresh" command-line-args))
  (delete "-refresh" command-line-args)
  (package-refresh-contents))

(package-initialize)

;;; Packages

(use-package cl-lib)

(use-package ample-theme
  :ensure t)

(use-package expand-region
  :bind ("C-c e" . er/expand-region)
  :ensure t)

(use-package undo-tree
  :init (global-undo-tree-mode 1)
  :bind (("C-c j" . undo-tree-undo)
         ("C-c k" . undo-tree-redo)
         ("C-c l" . undo-tree-switch-branch)
         ("C-c ;" . undo-tree-visualize))
  :ensure t)

(use-package auto-indent-mode
  :init (progn
          (add-hook 'prog-mode-hook 'auto-indent-mode)
          (add-hook 'prog-mode-hook (defun indent-tabs-mode-off ()
                                      (interactive)
                                      (setq indent-tabs-mode nil))))
  :ensure t)

(use-package highlight-parentheses
  :config (progn
	    (add-hook 'prog-mode-hook 'highlight-parentheses-mode))
  :ensure t)

(use-package flycheck
  :config (progn
            (add-hook 'c-mode-hook 'flycheck-mode)
            (add-hook 'c++-mode-hook 'flycheck-mode))
  :ensure t)

(use-package shell-command
  :ensure t)

;; Actually depends on shell-command
;(use-package bash-completion
;  :ensure t)

(use-package popwin
  :config (popwin-mode t)
  :ensure t)

(use-package markdown-mode
  :defer t
  :ensure t)
