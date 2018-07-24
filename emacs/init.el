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

;; Remove VC stuff from mode line
(setq-default mode-line-format
              '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified
                mode-line-remote mode-line-frame-identification mode-line-buffer-identification
                "   " mode-line-position "   " mode-line-modes mode-line-misc-info
                mode-line-end-spaces))

;; Fix default JS indent (for JSON files)
(setq-default js-indent-level 2)

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

;; This warning is super annoying
(unless (boundp 'warning-suppress-types)
  (setq warning-suppress-types nil))
(push '(undo discard-info) warning-suppress-types)

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

;; Toggle between the beginning of indentation and the beginning of the line
(global-set-key [remap move-beginning-of-line]
                (lambda (arg)
                  (interactive "^p")
                  (setq arg (or arg 1))
                  (when (/= arg 1)
                    (let ((line-move-visual nil))
                      (forward-line (1- arg))))
                  (let ((orig-point (point)))
                    (back-to-indentation)
                    (when (= orig-point (point))
                      (move-beginning-of-line 1)))))

;; Better backspace during isearch
;; See also: http://endlessparentheses.com/better-backspace-during-isearch.html
(define-key isearch-mode-map (kbd "<backspace>")
  (lambda ()
    (interactive)
    (if (= 0 (length isearch-string))
        (ding)
      (setq isearch-string
            (substring isearch-string 0 (or (isearch-fail-pos) (1- (length isearch-string)))))
      (setq isearch-message
            (mapconcat #'isearch-text-char-description isearch-string "")))
    (if isearch-other-end (goto-char isearch-other-end))
    (isearch-search)
    (isearch-push-state)
    (isearch-update)))

;; Open files with root
(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file (root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
(global-set-key (kbd "C-x C-r") #'sudo-edit)

;; Put backups and autosaves in separate directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Hook to fix fill-paragraph for Doxygen style comments
(let ((doxygen-comment-fix-hook
       (lambda ()
         (setq-local paragraph-start "^[ ]*\\(///\\|\\**\\)[ ]*\\([ ]*$\\|@\\)\\|^\f"))))
  (dolist (hook '(c-mode-common-hook js2-mode-hook))
    (add-hook hook doxygen-comment-fix-hook)))

;; Hook to fix C++ namespace indentation
(let ((namespace-indent-fix-hook
       (lambda ()
         (c-set-offset 'innamespace 0))))
  (add-hook 'c++-mode-hook namespace-indent-fix-hook))

;; Hook to fix missing space after trailing comments in Lisp
(let ((trailing-comment-fix-hook
       (lambda ()
         (setq comment-start "; "))))
  (dolist (hook '(lisp-mode-hook lisp-interaction-hook emacs-lisp-mode-hook))
    (add-hook hook trailing-comment-fix-hook)))

;; Hook to limit line length for Python (PEP 8)
(let ((pep8-line-length-fix-hook
       (lambda ()
         ;; Reload whitespace-mode
         (whitespace-mode -1)
         (setq fill-column 79)
         (whitespace-mode))))
  (add-hook 'python-mode-hook pep8-line-length-fix-hook))

;; Hook to fix s-exp movement in Python
(let ((python-sexp-fix-hook
       (lambda ()
         (setq forward-sexp-function nil))))
  (add-hook 'python-mode-hook python-sexp-fix-hook))

;; Hooks to display functions in some modes
(add-hook 'prog-mode-hook #'which-func-mode)

;; Hook to find other files in C and C++
(global-set-key (kbd "C-M-o") nil)
(setq-default ff-ignore-include t)
(let ((find-other-files-hook
       (lambda ()
         (local-set-key (kbd "C-M-o") #'ff-get-other-file))))
  (dolist (hook '(c-mode-hook c++-mode-hook))
    (add-hook hook find-other-files-hook)))

;; Monkey patch `vc-git-conflicted-files' because it sucks!
;; See Emacs bug #21559: http://debbugs.gnu.org/cgi/bugreport.cgi?bug=21559
(message "Monkey patching `vc-git-conflicted-files' for Emacs bug #21559!")
(require 'vc)
(defun vc-git-conflicted-files (directory)
  "Return the list of files with conflicts in DIRECTORY."
  (let* ((status
          (vc-git--run-command-string directory "diff-files" "--name-status"))
         (lines (when status (split-string status "\n" 'omit-nulls)))
         files)
    ;; TODO: Look into reimplementing `vc-git-state', as well as
    ;; `vc-git-dir-status-files', based on this output, thus making the
    ;; extra process call in `vc-git-find-file-hook' unnecessary.
    (dolist (line lines files)
      (when (string-match "\\([ MADRCU?!]\\)[ \t]+\\(.+\\)" line)
        (let ((state (match-string 1 line))
              (file (match-string 2 line)))
          ;; See git-status(1).
          (when (equal state "U")
            (push (expand-file-name file directory) files)))))))

;;; Package Support

(let ((default-directory "~/.emacs.d/plugins/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(setq use-package-always-pin "melpa-stable")

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

(use-package solarized-theme
  :if window-system
  :config (progn
            (setq solarized-emphasize-indicators nil)
            (setq x-underline-at-descent-line t)
            (load-theme 'solarized-dark t))
  :ensure t)

(use-package ample-theme
  :disabled t)

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
                  ido-create-new-buffer 'always
                  ido-default-buffer-method 'selected-window)
            (ido-mode)))

(use-package ido-completing-read+
  :config (ido-ubiquitous-mode)
  :ensure t)

(use-package smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c M-x" . execute-extended-command))
  :ensure t)

(use-package avy
  :bind (("C-c SPC" . avy-goto-char-2)
         ("C-c S-SPC" . avy-goto-line))
  :config (setq avy-style 'at-full)
  :ensure t)

(use-package ace-window
  :bind (("C-c o" . ace-window))
  :config (progn
            (ace-window-display-mode)
            (setq aw-background nil
                  aw-leading-char-style 'path)
            ;; Make the font look more like avy-jump
            (face-spec-set 'aw-leading-char-face
                           '((t (:foreground "white" :background "#e52b50")))))
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

;; Scratch doesn't appear to be downloadable right now...
;; (use-package scratch
;;   :commands scratch
;;   :ensure t)

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

(use-package expand-region
  :bind ("C-=" . er/expand-region)
  :ensure t)

(use-package anzu
  :defer 4
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config (global-anzu-mode)
  :diminish anzu-mode
  :ensure t)

(use-package ag
  :commands ag
  :ensure t)

(use-package wgrep
  :ensure t)

(use-package wgrep-ag
  :ensure t)

;; Some help for Vim users
(use-package evil
  :commands evil-mode
  :ensure t)

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
                        ;; Using nil means use fill-column
                        whitespace-line-column nil)
  :diminish whitespace-mode)

(use-package subword
  :config (global-subword-mode)
  :diminish subword-mode)

(use-package highlight-parentheses
  ;; This isn't installed through ELPA, because I want to use my fork
  :commands highlight-parentheses-mode
  :init (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
  :config (setq hl-paren-highlight-adjacent t
                blink-matching-paren nil)
  :diminish highlight-parentheses-mode)

(use-package irony
  :commands irony-mode
  :init (dolist (hook '(c-mode-hook c++-mode-hook))
          (add-hook hook #'irony-mode))
  :config (progn
            (add-hook 'irony-mode-hook
                      (lambda ()
                        (irony-cdb-autosetup-compile-options)))
            ;; For some reason Irony is not detecting C++11, so force it
            (setq-default irony-additional-clang-options '("-std=c++11")))
  :ensure t)

(use-package flycheck-irony
  :commands flycheck-irony-setup
  :ensure t)

(use-package flycheck
  :commands flycheck-mode
  :init (dolist (hook '(c-mode-hook c++-mode-hook js2-mode-hook))
          ;; Javascript requires JSHint
          (add-hook hook #'flycheck-mode))
  :config (progn
            (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
            (setq-default flycheck-clang-language-standard "c++11"
                          flycheck-gcc-language-standard "c++11"))
  :ensure t)

(use-package yasnippet
  :commands yas-minor-mode
  :init (dolist (hook '(lisp-mode-hook
                        lisp-interaction-hook
                        c-mode-hook
                        c++-mode-hook
                        js2-mode-hook
                        web-mode-hook))
          (add-hook hook #'yas-minor-mode))
  :config (progn
            (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
            (yas-reload-all))
  :ensure t)

(use-package lua-mode
  :mode ("\\.lua\\'" . lua-mode)
  :config (setq lua-indent-level 2)
  :ensure t)

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :config (progn
            (setq js2-basic-offset 2
                  js2-indent-switch-body t)
            (add-hook 'js2-mode-hook
                      (lambda ()
                        ;; Rename the terrible Javascript-IDE mode name
                        (setq mode-name "Js2"
                              comment-start "// "))))
  :ensure t)

(use-package json-mode
  :mode ("\\.json\\'" . json-mode)
  ;; TODO Disable this mode when there are really long lines (it's super slow)
  :config (setq json-reformat:indent-width 2)
  :ensure t)

(use-package web-mode
  ;; I'm mostly using this for ReactJS
  :mode ("\\.jsx\\'" . web-mode)
  :config (progn
            (setq web-mode-code-indent-offset 2
                  web-mode-markup-indent-offset 2)
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

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :config (add-hook 'markdown-mode-hook
                    (lambda ()
                      (flyspell-mode)
                      (flyspell-buffer)
                      (auto-fill-mode)))
  :ensure t)

(use-package rethink
  :commands set-rethink-lisp-indent
  :init (dolist (hook '(lisp-mode-hook lisp-interaction-hook))
          (add-hook hook #'set-rethink-lisp-indent)))

(use-package ori-emacs
  :bind ("<f5>" . ori-oclang-format)
  :config (ori-setup-vars))

(use-package google-c-style
  :commands google-set-c-style
  :init (add-hook 'c-mode-common-hook
                  (lambda ()
                    (google-set-c-style)
                    (c-set-offset 'inextern-lang 0)))
  :ensure t)

(use-package comint
  :config (progn
            ;; Unbind some of the C-c keys comint-mode obnoxiously binds...
            (define-key comint-mode-map (kbd "C-c SPC") nil)))

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

(use-package magit
  :bind (("<f9>" . magit-status)
         ("<f10>" . magit-blame)
         ("<f11>" . magit-log-buffer-file))
  :config (setq magit-completing-read-function 'magit-ido-completing-read
                magit-revert-buffers t
                magit-push-always-verify nil
                magit-pull-arguments '("--rebase")
                git-commit-fill-column 70)
  :ensure t)

(use-package git-timemachine
  :bind ("<f12>" . git-timemachine)
  :ensure t)

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :config (setq python-shell-interpreter "python3"))

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
