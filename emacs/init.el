;;; Personal Customizations

(mapc (lambda(mode) (when (fboundp mode) (apply mode '(-1))))
      '(tool-bar-mode
        menu-bar-mode
        scroll-bar-mode))

(setq ring-bell-function #'ignore
      inhibit-startup-screen t)
(setq-default indent-tabs-mode nil
              tab-width 4)

(icomplete-mode)

;; Auto revert everything (including TAGS)
(global-auto-revert-mode)
(setq tags-revert-without-query t)

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

;; Don't open logs with nroff 
(add-to-list 'auto-mode-alist '("\\.[0-9]+\\'" . fundamental-mode))

;; Add Rethink Lisp indenting... is there a better way to do this?
(add-to-list 'load-path "~/.emacs.d/")
(require 'rethink)
(set-rethink-lisp-indent)

;; Allow C-x C-o to go to the last window
(global-set-key (kbd "C-x C-o")
                '(lambda ()
                   (interactive)
                   (other-window -1)))

;; Allow C-x p and C-x C-p to switch frames
(global-set-key (kbd "C-x p")
                '(lambda ()
                   (interactive)
                   (other-frame 1)
                   (select-frame-set-input-focus (selected-frame))))
(global-set-key (kbd "C-x C-p")
                '(lambda ()
                   (interactive)
                   (other-frame -1)
                   (select-frame-set-input-focus (selected-frame))))

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

;; Secret option to refresh packages
(let ((refresh-packages-option "--refresh-packages"))
  (when (member refresh-packages-option command-line-args)
    (delete refresh-packages-option command-line-args)
    (package-refresh-contents)))

(package-initialize)

;;; Packages

(use-package cl-lib)

(use-package solarized-theme
  :init (progn
          (setq solarized-use-less-bold t)
          (setq solarized-emphasize-indicators nil)
          (setq x-underline-at-descent-line t))
  :config (load-theme 'solarized-dark t)
  :ensure t)

(use-package ample-theme
  :disabled t
  :ensure t)

(use-package expand-region
  :bind ("C-c e" . er/expand-region)
  :ensure t)

(use-package undo-tree
  :init (global-undo-tree-mode)
  :bind (("C-c j" . undo-tree-undo)
         ("C-c k" . undo-tree-redo)
         ("C-c l" . undo-tree-switch-branch)
         ("C-c ;" . undo-tree-visualize))
  :ensure t)

(use-package auto-indent-mode
  :defer t
  :init (add-hook 'prog-mode-hook 'auto-indent-mode)
  :config (setq auto-indent-blank-lines-on-move nil)
  :ensure t)

(use-package highlight-parentheses
  :defer t
  :init (add-hook 'prog-mode-hook 'highlight-parentheses-mode)
  :ensure t)

(use-package flycheck
  :defer t
  :init (progn
          (add-hook 'c-mode-hook 'flycheck-mode)
          (add-hook 'c++-mode-hook 'flycheck-mode))
  :ensure t)

(use-package shell-command
  :init (shell-command-completion-mode)
  :ensure t)

;; Actually depends on shell-command
(use-package bash-completion
  :disabled t
  :init (bash-completion-setup)
  :ensure t)

(use-package popwin
  :init (popwin-mode)
  :ensure t)

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :ensure t)
