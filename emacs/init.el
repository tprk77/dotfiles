;;; Personal Customizations

(mapc (lambda (mode) (when (fboundp mode) (apply mode '(-1))))
      '(tool-bar-mode
        menu-bar-mode
        scroll-bar-mode))

(set-face-attribute 'default nil :height 100)
(setq ring-bell-function #'ignore
      inhibit-startup-screen t)
(setq-default indent-tabs-mode nil
              tab-width 4)

(icomplete-mode)

;; Auto revert everything (including TAGS)
(global-auto-revert-mode)
(setq tags-revert-without-query t)

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
                (lambda ()
                  (interactive)
                  (other-window -1)))

;; Allow C-x p and C-x C-p to switch frames
(global-set-key (kbd "C-x p")
                (lambda ()
                  (interactive)
                  (other-frame 1)
                  (select-frame-set-input-focus (selected-frame))))
(global-set-key (kbd "C-x C-p")
                (lambda ()
                  (interactive)
                  (other-frame -1)
                  (select-frame-set-input-focus (selected-frame))))

;; Put backups and autosaves in separate directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Hook for C++
(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-offset 'innamespace 0)))

;; Hook for Javascript
(add-hook 'js-mode-hook
          (lambda ()
            (setq js-indent-level 2)))

;;; Package Support

(add-to-list 'load-path "~/.emacs.d/use-package/")
(let ((default-directory "~/.emacs.d/plugins/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(require 'use-package)
(font-lock-add-keywords 'emacs-lisp-mode use-package-font-lock-keywords)
(font-lock-add-keywords 'lisp-interaction-mode use-package-font-lock-keywords)

;; Secret option to refresh packages
(let ((refresh-packages-option "--refresh-packages"))
  (when (member refresh-packages-option command-line-args)
    (delete refresh-packages-option command-line-args)
    (package-refresh-contents)))

(package-initialize)

;;; Packages

(use-package cl-lib)

(use-package custom
  :config (setq custom-file "~/.emacs.d/custom.el"))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward
                uniquify-after-kill-buffer-p t))

(use-package whitespace
  :init (add-hook 'prog-mode-hook 'whitespace-mode)
  :config (setq-default whitespace-style '(face
                                           trailing
                                           tabs
                                           spaces
                                           lines-tail
                                           newline
                                           indentation
                                           empty
                                           space-mark
                                           tab-mark
                                           newline-mark)
                        whitespace-line-column 100))

(use-package solarized-theme
  :config (progn
            (setq solarized-emphasize-indicators nil)
            (setq x-underline-at-descent-line t)
            (load-theme 'solarized-dark t))
  :ensure t)

(use-package ample-theme
  :disabled t)

;; Going to load first thing with scratch buffer
(use-package auto-indent-mode
  :init (add-hook 'prog-mode-hook 'auto-indent-mode)
  :config (setq auto-indent-blank-lines-on-move nil)
  :ensure t)

;; Going to load first thing with scratch buffer
(use-package highlight-parentheses
  ;; We want to use my fork for this, so no ensure
  :init (add-hook 'prog-mode-hook 'highlight-parentheses-mode)
  :config (setq hl-paren-highlight-adjacent t
                blink-matching-paren nil))

(use-package flycheck
  :init (progn
          (add-hook 'c-mode-hook 'flycheck-mode)
          (add-hook 'c++-mode-hook  'flycheck-mode))
  :config (add-hook 'c++-mode-hook
                    (lambda ()
                      (setq flycheck-clang-language-standard "c++11"
                            flycheck-gcc-language-standard "c++11")))
  :defer t
  :ensure t)

(use-package google-c-style
  :init (add-hook 'c-mode-common-hook 'google-set-c-style)
  :defer t
  :ensure t)

(use-package popwin
  :idle (popwin-mode)
  :idle-priority 2
  ;; Popwin appears to be missing this autoload
  :commands popwin-mode
  :ensure t)

(use-package undo-tree
  :idle (global-undo-tree-mode)
  :idle-priority 3
  :bind (("C-c j" . undo-tree-undo)
         ("C-c k" . undo-tree-redo)
         ("C-c l" . undo-tree-switch-branch)
         ("C-c ;" . undo-tree-visualize))
  :ensure t)

(use-package bash-completion
  :defer t
  :ensure t)

(use-package shell-command
  :idle (shell-command-completion-mode)
  :idle-priority 4
  :config (bash-completion-setup)
  :defer t
  :ensure t)

(use-package expand-region
  :bind ("C-c e" . er/expand-region)
  :ensure t)

(use-package markdown-mode
  :config (add-hook 'markdown-mode-hook
                    (lambda ()
                      (flyspell-mode)
                      (flyspell-buffer)
                      (auto-fill-mode)))
  :defer t
  :ensure t)
