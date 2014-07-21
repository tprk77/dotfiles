(mapc (lambda(mode) (when (fboundp mode) (apply mode '(-1))))
      '(tool-bar-mode
        menu-bar-mode
        scroll-bar-mode))

(setq ring-bell-function #'ignore
      inhibit-startup-screen t
      indent-tabs-mode nil)

(icomplete-mode t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-after-kill-buffer-p t)

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

(use-package cl-lib)

(use-package ample-theme
  :ensure t)

(use-package expand-region
  :bind ("C-c e" . er/expand-region)
  :ensure t)

(use-package popwin
  :config (popwin-mode t)
  :ensure t)

(use-package markdown-mode
  :defer t
  :ensure t)
