;;; Personal Customizations

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq ring-bell-function #'ignore
      inhibit-startup-screen t
      initial-major-mode #'text-mode
      initial-scratch-message nil)

(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 100
              sentence-end-double-space nil
              require-final-newline t)

;; P.S. Use customize for face stuff...

;; Auto revert everything (including TAGS)
(global-auto-revert-mode)
(setq tags-revert-without-query t)

;; Use column numbers
(column-number-mode)

;; Smoother scrolling
(setq mouse-wheel-scroll-amount '(4 ((shift) . 4)) ; four lines at a time
      mouse-wheel-progressive-speed nil ; don't accelerate scrolling
      mouse-wheel-follow-mouse t ; scroll window under mouse
      scroll-step 1 ; keyboard scroll one line at a time
      scroll-conservatively 10000 ; don't jump around as much
      auto-window-vscroll nil) ; magic?

;; Don't open logs with nroff
(add-to-list 'auto-mode-alist '("\\.[0-9]+\\'" . #'fundamental-mode))

;; Typing y is good enough for yes
(fset 'yes-or-no-p 'y-or-n-p)

;; Don't confirm new buffers
(setq confirm-nonexistent-file-or-buffer nil)

;; Don't prompt when there is a process attached to a buffer
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Don't prompt for active process with shells
(add-hook 'shell-mode-hook
          (lambda ()
            (set-process-query-on-exit-flag
             (get-buffer-process (current-buffer))
             nil)))

;; Do prompt to kill emacs!
(setq confirm-kill-emacs 'y-or-n-p)

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

;; Multiframe movement with easy bindings
(global-set-key (kbd "M-o") #'next-multiframe-window)
(global-set-key (kbd "M-O") #'previous-multiframe-window)

;; Some function key bindings
(global-set-key (kbd "<f1>") #'shell)
(global-set-key (kbd "<f2>") #'rgrep)

;; Put backups and autosaves in separate directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Hook for C++
(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-offset 'innamespace 0)))

;; Hook for Javascript
(add-hook 'js-mode-hook
          (lambda ()
            (setq js-indent-level 2)))

;; Hooks for various Lisp modes
(let ((lisp-hook
       (lambda ()
         ;; Fix annoying missing space after comment
         (setq comment-start "; "))))
  (dolist (hook '(lisp-mode-hook lisp-interaction-hook emacs-lisp-mode-hook))
    (add-hook hook lisp-hook)))

;;; Package Support

(let ((default-directory "~/.emacs.d/plugins/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; For editing use-package definitions
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
  :config (progn
            (setq custom-file "~/.emacs.d/custom.el")
            (load custom-file)))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward
                uniquify-after-kill-buffer-p t))

(use-package icomplete
  :config (icomplete-mode))

(use-package ido
  :config (progn
            (setq ido-everywhere t
                  ido-enable-flex-matching t
                  ido-auto-merge-work-directories-length -1
                  ido-create-new-buffer 'always)
            (ido-mode)))

(use-package ido-ubiquitous
  :config (ido-ubiquitous-mode)
  :ensure t)

(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c M-x" . execute-extended-command))
  :ensure t)

(use-package ace-window
  :bind (("C-c o" . ace-window)
         ("C-c SPC" . avi-goto-char)
         ("C-c S-SPC" . avi-goto-line))
  :config (ace-window-display-mode)
  :ensure t)

(use-package solarized-theme
  :if window-system
  :config (progn
            (setq solarized-emphasize-indicators nil)
            (setq x-underline-at-descent-line t)
            (load-theme 'solarized-dark t))
  :ensure t)

(use-package ample-theme
  :disabled t)

(use-package rethink
  :commands set-rethink-lisp-indent
  :init (dolist (hook '(lisp-mode-hook lisp-interaction-hook))
            (add-hook hook #'set-rethink-lisp-indent)))

(use-package whitespace
  :commands whitespace-mode
  :init (add-hook 'prog-mode-hook #'whitespace-mode)
  :config (setq-default whitespace-style
                        (if window-system
                            '(face
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
                          '(face
                            trailing
                            lines-tail
                            empty
                            space-mark
                            tab-mark
                            newline-mark))
                        whitespace-line-column 100)
  :diminish whitespace-mode)

(use-package auto-indent-mode
  :commands auto-indent-mode
  :init (progn
          (add-hook 'prog-mode-hook #'auto-indent-mode)
          ;; Make C-backspace act like normal backspace, regardless of auto-indent
          (global-set-key (kbd "<C-backspace>")
                          (lookup-key (current-global-map) (kbd "DEL"))))
  :config (progn
            (setq auto-indent-assign-indent-level-variables nil
                  auto-indent-blank-lines-on-move nil
                  auto-indent-backward-delete-char-behavior 'hungry)
            (defun backward-delete-no-auto-indent (arg)
              "Do what backspace would normally do, but with auto-indent set to `untabify'."
              (interactive "p")
              (let ((auto-indent-backward-delete-char-behavior 'untabify))
                (funcall (key-binding (kbd "DEL")) arg)))
            (global-set-key (kbd "<C-backspace>") #'backward-delete-no-auto-indent))
  :ensure t)

(use-package highlight-parentheses
  ;; This isn't installed through ELPA, because I want to use my fork
  :commands highlight-parentheses-mode
  :init (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
  :config (setq hl-paren-highlight-adjacent t
                blink-matching-paren nil)
  :diminish highlight-parentheses-mode)

(use-package flycheck
  :commands flycheck-mode
  :init (progn
          (add-hook 'c-mode-hook #'flycheck-mode)
          (add-hook 'c++-mode-hook  #'flycheck-mode)
          ;; Javascript requires JSHint
          (add-hook 'js-mode-hook  #'flycheck-mode)
          (add-hook 'js2-mode-hook  #'flycheck-mode))
  :config (add-hook 'c++-mode-hook
                    (lambda ()
                      (setq flycheck-clang-language-standard "c++11"
                            flycheck-gcc-language-standard "c++11")))
  :ensure t)

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :config (progn
            (setq-default js2-basic-offset 2)
            (add-hook 'js2-mode-hook
                      (lambda ()
                        ;; Rename the terrible Javascript-IDE mode name
                        (setq mode-name "Js2"
                              comment-start "// "))))
  :ensure t)

(use-package web-mode
  ;; I'm mostly using this for ReactJS
  :mode ("\\.jsx\\'" . web-mode)
  :config (progn
            (flycheck-define-checker jsxhint-checker
              "A JSX syntax and style checker based on JSXHint."
              :command ("jsxhint" source)
              :error-patterns ((error line-start (1+ nonl) ": line " line
                                      ", col " column ", " (message) line-end))
              :modes (web-mode))
            (add-hook 'web-mode-hook
                      (lambda ()
                        (when (string= web-mode-content-type "jsx")
                          (flycheck-select-checker 'jsxhint-checker)
                          (flycheck-mode)))))
  :ensure t)

(use-package google-c-style
  :commands google-set-c-style
  :init (add-hook 'c-mode-common-hook #'google-set-c-style)
  :ensure t)

(use-package popwin
  :defer 1
  :commands popwin-mode
  :config (popwin-mode)
  :ensure t)

(use-package buffer-move
  :bind (("C-S-<up>" . buf-move-up)
         ("C-S-<down>" . buf-move-down)
         ("C-S-<left>" . buf-move-left)
         ("C-S-<right>" . buf-move-right))
  :ensure t)

(use-package undo-tree
  :defer 2
  :commands global-undo-tree-mode
  :bind (("C-c j" . undo-tree-undo)
         ("C-c k" . undo-tree-redo)
         ("C-c l" . undo-tree-switch-branch)
         ("C-c ;" . undo-tree-visualize))
  :config (global-undo-tree-mode)
  :diminish undo-tree-mode
  :ensure t)

(use-package bash-completion
  :commands bash-completion-setup
  :ensure t)

(use-package shell-command
  :defer 3
  :commands shell-command-completion-mode
  :config (progn
            (shell-command-completion-mode)
            (bash-completion-setup))
  :ensure t)

(use-package shell-select
  :bind ("<f1>" . shell-select-dwim))

(use-package expand-region
  :bind ("C-=" . er/expand-region)
  :ensure t)

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :config (add-hook 'markdown-mode-hook
                    (lambda ()
                      (flyspell-mode)
                      (flyspell-buffer)
                      (auto-fill-mode)))
  :ensure t)

;; Some help for Vim users
(use-package evil
  :commands evil-mode
  :ensure t)

(use-package yasnippet
  :commands yas-minor-mode
  :init (progn
          (add-hook 'lisp-mode-hook #'yas-minor-mode)
          (add-hook 'lisp-interaction-mode-hook #'yas-minor-mode)
          (add-hook 'c-mode-hook #'yas-minor-mode)
          (add-hook 'c++-mode-hook #'yas-minor-mode)
          (add-hook 'js-mode-hook #'yas-minor-mode)
          (add-hook 'js2-mode-hook #'yas-minor-mode))
  :config (progn
            (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
            (yas-reload-all))
  :ensure t)

(use-package magit
  ;; This isn't installed through ELPA, because I want to use the "next" branch
  :commands (magit-status magit-blame)
  :bind (("<f9>" . magit-status))
  :config (setq magit-completing-read-function 'magit-ido-completing-read))

(use-package slime
  :disabled t
  :defer t
  :config (progn
            (setq inferior-lisp-program "/usr/bin/sbcl")
            ;; Not sure why I need to call this...
            (slime-setup '(slime-fancy
                           slime-asdf)))
  :ensure t)

(use-package rosemacs
  :if (file-exists-p "/opt/ros/indigo/share/emacs/site-lisp")
  :load-path "/opt/ros/indigo/share/emacs/site-lisp"
  :commands invoke-rosemacs
  :config (progn
            (setq ros-completion-function #'ido-completing-read)
            (global-set-key (kbd "C-x C-r") ros-keymap)))
