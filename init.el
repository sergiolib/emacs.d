(setq backup-directory-alist `(("." . "~/.saves")))
(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'none))
(when (eq system-type 'gnu/linux)
  (setenv "PATH" (concat
		  "/home/sliberman/.local/bin/:"
		  "/home/sliberman/.pyenv/bin/:"
		  "/home/sliberman/.pyenv/versions/3.12.7/bin/:"
		  (getenv "PATH"))))
(add-to-list 'exec-path "/home/sliberman/.local/bin/")

(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; (setq use-package-verbose 1)
(setq use-package-always-ensure t)

(use-package emacs
  :ensure nil
  :config
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode 1)
  (setq tab-width 4)
  (add-hook 'typescript-ts-base-mode-hook #'(lambda () (setq tab-width 2)))
  (setq inhibit-startup-message t)
  (setq initial-scratch-message nil)
  (setq ring-bell-function #'ignore)
  (tool-bar-mode -1)
  (when (eq system-type 'gnu/linux)
    (menu-bar-mode -1))
  (when (eq system-type 'darwin)
    (menu-bar-mode 1))
  (scroll-bar-mode -1)
  (blink-cursor-mode 1)
  (when (eq system-type 'gnu/linux)
    (set-face-attribute 'default nil :height 110 :family "JetBrains Mono"))
  (when (eq system-type 'darwin)
    (set-face-attribute 'default nil :height 140 :family "JetBrains Mono"))
  (when (not (display-graphic-p)) (xterm-mouse-mode 1))
  (auto-save-visited-mode -1)
  (auto-revert-mode 1)
  (setq calendar-week-start-day 1)
  (recentf-mode 1)
  (global-set-key (kbd "<home>") 'back-to-indentation)
  (global-set-key (kbd "<end>") 'end-of-line)
  (setq show-paren-style 'expression)
  (setq tab-always-indent 'complete)
  (electric-pair-mode t)
  (setq display-line-numbers-type 'relative
	display-line-numbers-widen t)
  (global-display-line-numbers-mode 1)
  (setq excluded-hooks-from-display-numbers '(doc-view-mode-hook))
  (mapc (lambda (hook) (add-hook hook
				 #'(lambda () (display-line-numbers-mode 0))))
	excluded-hooks-from-display-numbers)
  (let ((no-line-number-hooks '(vterm-mode-hook help-mode-hook)))
    (dolist (hook no-line-number-hooks)
      (add-hook hook #'(lambda () (display-line-numbers-mode -1)))))
  (windmove-default-keybindings '(C S))
  (winner-mode 1)
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
  (add-hook 'compilation-filter-hook 'ansi-osc-compilation-filter)
  (add-hook 'typescript-ts-mode-hook #'(lambda () (setq-local js-indent-level 2)))
  (add-hook 'tsx-ts-mode-hook #'(lambda () (setq-local js-indent-level 2)))
  (add-hook 'typescript-ts-mode-hook #'(lambda () (setq-local js-jsx-indent-level 2)))
  (add-hook 'tsx-ts-mode-hook #'(lambda () (setq-local js-jsx-indent-level 2)))
  (unbind-key (kbd "C-x C-z"))
  (unbind-key (kbd "C-z"))
  (global-set-key (kbd "C-c e") 'open-init-file)
  (setq custom-file "~/.emacs.d/custom.el")
  (setq completion-ignore-case t
	read-file-name-completion-ignore-case t
	read-buffer-completion-ignore-case t)
  :mode
  ("\\.tsx\\'" . tsx-ts-mode)
  ("\\.ts\\'" . typescript-ts-mode)
  :init
  (defun open-init-file ()
    "Open the init file"
    (interactive)
    (find-file "~/.emacs.d/init.el"))
  (when (eq system-type 'gnu/linux)
    (setenv "PATH" (concat "/Library/TeX/texbin/:" (getenv "PATH")))))

(use-package ef-themes
  :config
  (load-theme 'ef-elea-dark t))

(use-package vertico
  :ensure (:tag "1.9")
  :config
  (vertico-mode 1)
  :custom
  (vertico-cycle t))

(use-package vertico-mouse
  :after vertico
  :ensure nil
  :config
  (vertico-mouse-mode 1))

(use-package vertico-repeat
  :after vertico
  :ensure nil
  :bind	("C-c r" . 'vertico-repeat))

(use-package marginalia
  :ensure (:tag "1.7")
  :config
  (marginalia-mode 1))

(use-package embark
  :ensure (:tag "1.1")
  :custom
  (embark-quit-after-action nil)
  :bind
  ("C-." . embark-act))

(use-package consult
  :ensure (:tag "1.8")  
  :bind
  ("C-s" . consult-line)
  ("C-x b" . consult-buffer)
  ("C-x ," . consult-recent-file)
  ("M-y" . consult-yank-pop))

(use-package embark-consult
  :after embark)

(use-package orderless
  :ensure (:tag "1.1")
  :custom
  (completion-styles '(orderless basic)))

(use-package corfu
  :ensure (:tag "1.5")
  :init
  (global-corfu-mode 1)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 3)
  (corfu-popupinfo-mode 1)
  :config
  (corfu-echo-mode 1)
  (corfu-history-mode 1)
  (define-key corfu-map (kbd "TAB") 'corfu-insert))

(use-package doom-modeline
  :ensure (:tag "v4.1.0")
  :config
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-modal-icon nil)
  (doom-modeline-modal-modern-icon nil)
  )

(use-package all-the-icons
  :ensure (:tag "5.0.0"))

(use-package nerd-icons-corfu
  :ensure (:tag "v0.4.2")
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package dired
  :commands (dired)
  :ensure nil
  :config
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package vterm)

(use-package vterm-toggle
  :bind
  ("<f8>" . vterm-toggle)
  :config
  (setq vterm-toggle-scope 'project)
  (add-hook 'vterm-toggle-show-hook #'(lambda () (define-key vterm-mode-map (kbd "<f8>") 'vterm-toggle)))
  (add-to-list 'display-buffer-alist '("\\*vterm\\*" (display-buffer-reuse-window display-buffer-at-bottom) (window-height . 0.3))))

(use-package treemacs
  :commands (treemacs))

(use-package project
  :ensure nil
  :config
  (defun sergio/project-vterm ()
    (interactive)
    (let ((default-directory (project-root (project-current))))
      (vterm)))
  (defun sergio/project-magit ()
    (interactive)
    (let ((default-directory (project-root (project-current))))
      (magit-status)))
  (defun sergio/modify-dir-locals ()
    (interactive)
    (find-file (f-join (project-root (project-current)) ".dir-locals.el")))

  (defun sergio/consult-ripgrep ()
    (interactive)
    (consult-ripgrep (project-root (project-current))))

  (add-to-list 'project-switch-commands '(sergio/project-vterm "Vterm" "t"))
  (add-to-list 'project-switch-commands '(sergio/project-magit "Magit" "m"))
  (add-to-list 'project-switch-commands '(sergio/project-magit "Modify .dir-locals.el" "v"))
  (setq project-switch-commands (seq-filter (lambda (x)
					      (not (eq (car x)
						       'project-vc-dir)))
					    project-switch-commands))
  (define-key project-prefix-map (kbd "t") 'sergio/project-vterm)
  (define-key project-prefix-map (kbd "m") 'sergio/project-magit)
  (define-key project-prefix-map (kbd "v") 'sergio/modify-dir-locals)
  (define-key project-prefix-map (kbd "g") 'sergio/consult-ripgrep))

(use-package ruff-format
  :hook
  (python-mode . ruff-format-on-save-mode)
  (python-ts-mode . ruff-format-on-save-mode))

(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :init
  (add-hook 'python-base-mode-hook 'eglot-ensure 100)
  (add-hook 'terraform-mode-hook 'eglot-ensure 100)
  (add-hook 'tsx-ts-mode-hook 'eglot-ensure 100)
  (add-hook 'typescript-ts-mode-hook 'eglot-ensure 100)
  :config
  (setq eglot-events-buffer-size 0)
  (fset #'jsonrpc--log-event #'ignore)
  (setq gc-cons-threshold 800000
	read-process-output-max (* 1024 1024))
  (set-face-attribute 'eglot-diagnostic-tag-unnecessary-face nil :underline t :slant 'italic)
  (define-key eglot-mode-map (kbd "C-c l r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c l a") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c l =") 'eglot-format-buffer)
  (define-key eglot-mode-map (kbd "C-c l e") 'flymake-show-buffer-diagnostics))

(use-package eglot-booster
  :after eglot
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :config (eglot-booster-mode))

(use-package python
  :ensure nil
  :config
  (add-hook 'python-base-mode-hook 'pyvenv-mode)
  (add-hook 'python-base-mode-hook 'poetry-tracking-mode 10)
  :mode
  ("\\.py\\'" . python-ts-mode))


(use-package pyvenv
  :commands (pyvenv-mode)
  )

(use-package poetry
  :commands (poetry-tracking-mode poetry))

(use-package terraform-mode
  :mode "\\.tf\\'")

(use-package cape
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (defun add-cape-file-to-python-capfs ()
    (add-hook 'completion-at-point-functions #'cape-file nil t)
    (remove-hook 'completion-at-point-functions 'python-completion-at-point t)
    (add-hook 'completion-at-point-functions #'python-completion-at-point nil t)
    )
  (defun add-cape-file-to-tf-capfs ()
    (add-hook 'completion-at-point-functions #'cape-file nil t))
  (defun add-cape-file-to-lisp-data-capfs ()
    (add-hook 'completion-at-point-functions #'cape-file nil t))
  (add-hook 'python-base-mode-hook #'add-cape-file-to-python-capfs)
  (add-hook 'lisp-data-mode-hook #'add-cape-file-to-lisp-data-capfs)
  (add-hook 'terraform-mode-hook #'add-cape-file-to-tf-capfs))

(use-package which-key
  :config
  (which-key-mode 1))

(use-package dockerfile-mode
  :mode
  ("\\Dockerfile\\'"))

(use-package magit
  :ensure (:tag "v3.3.0")
  :bind
  ("C-c g" . magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package yaml-ts-mode
  :ensure nil
  :mode
  ("\\.ya?ml\\'" . yaml-ts-mode)
  :config
  (add-hook 'yaml-ts-mode-hook (lambda () (setq-local tab-width 2))))

(use-package direnv
  :config
  (direnv-mode 1)
  (setq direnv-always-show-summary nil))

(use-package code-cells
  :commands (code-cells-mode))

(use-package move-text
  :config
  (move-text-default-bindings))

(use-package git-gutter
  :config
  (global-git-gutter-mode 1))

;; (use-package yasnippet)

(use-package json-mode
  :mode
  ("\\.json\\'" . json-mode))

(use-package denote
  :bind
  ("C-c C-n" . denote-open-or-create)
  ("C-c n" . denote-open-or-create)
  :custom
  (denote-directory (if (eq system-type 'darwin)
			"~/Library/CloudStorage/GoogleDrive-sergiolib@gmail.com/My Drive/Notes/"
		      "~/Documents/Notes/")))

(use-package org
  :ensure nil
  :bind
  ("C-c a" . org-agenda-list)
  ("C-c c" . org-capture)
  ("C-c s" . org-store-link)
  :custom
  (org-indent-indentation-per-level 1)
  (rorg-edit-src-content-indentation 0)
  (org-agenda-files (if (eq system-type 'gnu/linux)
			(append (f-files "~/Documents/Notes" #'(lambda (f) (s-ends-with? ".org" f)) t)
				'("~/Insync/sergiolib@gmail.com/Google Drive/Agenda.org"))
		      (append '("~/Documents/agenda.org") (f-files "/Users/sliberman/Library/CloudStorage/GoogleDrive-sergiolib@gmail.com/My Drive/Notes/" #'(lambda (f) (s-ends-with? ".org" f)) t))))
  (org-default-notes-file "~/Insync/sergiolib@gmail.com/Google Drive/CapturedTasks.org")
  :hook
  (org-mode . org-indent-mode)
  (org-mode . (lambda () (electric-indent-local-mode -1)))
  :config
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sql" . "src sql"))
  (add-to-list 'org-structure-template-alist '("rest" . "src restclient"))
  (add-to-list 'org-structure-template-alist '("sh" . "src bash"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-hook
   'org-mode-hook
   (lambda ()
     (setq-local electric-pair-inhibit-predicate
		 `(lambda (c)
                    (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
  (require 'ob-sql)
  (org-babel-do-load-languages
   'org-babel-load-languages '((emacs-lisp . t)
			       (python . t)
			       (shell . t)
			       (sql . t)
			       (restclient . t))))

(use-package org-contrib
  :after org
  :config
  (require 'ox-extra)
  (ox-extras-activate '(latex-header-blocks ignore-headlines)))

(use-package avy
  :bind ("C-c C-SPC" . avy-goto-char))

(use-package editorconfig
  :config
  (add-to-list 'editorconfig-indentation-alist '(tsx-ts-mode . typescript-ts-mode-indent-offset))
  (add-hook 'tsx-ts-mode-hook #'editorconfig-apply)
  :hook (tsx-ts-mode . editorconfig-mode))

(use-package docker
  :bind
  ("C-c C-d d" . docker)
  ("C-c C-d c" . docker-compose)
  ("C-c C-d C-d" . docker)
  ("C-c C-d C-c" . docker-compose)
  :init
  (setq docker-compose-command "docker compose"))

(use-package rainbow-mode
  :hook
  (prog-mode . rainbow-mode))

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; (use-package expand-region
;;   :bind
;;   ("C-'" . 'er/expand-region))

(use-package restclient
  :commands (restclient-mode))

(use-package ob-restclient
  :after org)

(use-package wgrep
  :init
  (require 'wgrep))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; (use-package multiple-cursors
;;   :bind
;;   ("C->" . mc/mark-next-like-this)
;;   ("C-<" . mc/mark-previous-like-this))

(use-package protobuf-ts-mode
  :mode "\\.proto\\'")

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (global-set-key (kbd "C-M-u") 'universal-argument)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  :custom
  (evil-undo-system 'undo-redo)
  (evil-want-C-u-scroll t)
  (evil-want-fine-undo t))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-setup))

(use-package general
  :config
  (general-create-definer leader
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (leader
    "p" '(:keymap project-prefix-map :package project)
    "e" '(:ignore t :which-key "Emacs")
    "ee" '((lambda () (interactive) (find-file user-init-file)) :which-key "Visit init file")
    "b" '(:ignore t :which-key "Buffers")
    "bb" 'consult-buffer
    "," 'consult-recent-file))

(use-package docker-compose-mode
  :mode "compose.ya?ml")

(use-package evil-multiedit
  :config
  (evil-multiedit-default-keybinds)
  (evil-define-key '(insert normal visual) evil-multiedit-mode-map (kbd "RET") nil)
  (evil-define-key '(insert normal visual) evil-multiedit-mode-map (kbd "C-c d") 'evil-multiedit-toggle-or-restrict-region))

(use-package evil-nerd-commenter
  :bind
  ("M-;" . evilnc-comment-or-uncomment-lines))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package helpful
  :bind
  ("C-h v" . helpful-variable)
  ("C-h f" . helpful-function))

(use-package apheleia
  :config
  (apheleia-global-mode 1)
  (setq apheleia-formatters-respect-indent-level nil))

(use-package combobulate
  :commands (combobulate)
  :ensure (:host github :repo "mickeynp/combobulate"))

(setenv "ANDROID_HOME" "/Users/sliberman/Library/Android/sdk")

(use-package transient :ensure (:pin "0.7.4"))
(use-package gptel
  :config
  (setq
   gptel-model 'mistral:latest
   gptel-backend (gptel-make-ollama "Ollama"
		   :host "localhost:11434"
		   :stream t
		   :models '(mistral:latest))))
