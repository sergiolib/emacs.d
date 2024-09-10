(setq backup-directory-alist `(("." . "~/.saves")))
(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'none))

(defvar elpaca-installer-version 0.7)
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
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
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

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;;(setq use-package-verbose 1)
(setq use-package-always-ensure t)

;;Turns off elpaca-use-package-mode current declaration
;;Note this will cause evaluate the declaration immediately. It is not deferred.
;;Useful for configuring built-in emacs features.
(use-package emacs
  :ensure nil
  :config
  (setq-default cursor-type 'bar)
  (setq tab-width 4)
  ;;disable splash screen and startup message
  (setq inhibit-startup-message t)
  (setq initial-scratch-message nil)
  (setq ring-bell-function #'ignore)
  (tool-bar-mode -1)
  (menu-bar-mode 1)
  (scroll-bar-mode -1)
  (blink-cursor-mode -1)
  ;; (setq modus-themes-mode-line '(accented borderless 1.0))
  ;; (load-theme 'modus-operandi)
  (set-face-attribute 'default nil :height 160 :family "JetBrains Mono")
  (auto-save-visited-mode -1)
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
  (let ((no-line-number-hooks '(vterm-mode-hook help-mode-hook)))
    (dolist (hook no-line-number-hooks)
      (add-hook hook #'(lambda () (display-line-numbers-mode -1)))))
  (windmove-default-keybindings '(C S))
  (winner-mode 1)
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
  (add-hook 'compilation-filter-hook 'ansi-osc-compilation-filter)
  (add-hook 'tsx-ts-mode-hook 'eglot-ensure)
  (add-hook 'typescript-ts-mode-hook 'eglot-ensure)
  (add-hook 'typescript-ts-mode-hook #'(lambda () (setq-local js-indent-level 2)))
  (add-hook 'tsx-ts-mode-hook #'(lambda () (setq-local js-indent-level 2)))
  (add-hook 'typescript-ts-mode-hook #'(lambda () (setq-local js-jsx-indent-level 2)))
  (add-hook 'tsx-ts-mode-hook #'(lambda () (setq-local js-jsx-indent-level 2)))
  (defun my/frame-recenter (&optional frame)
    "Center FRAME on the screen.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame."
    (interactive)
    (unless (eq 'maximised (frame-parameter nil 'fullscreen))
      (modify-frame-parameters
       frame '((user-position . t) (top . 0.5) (left . 0.5)))))
  (when (window-system)
    (set-frame-height (selected-frame) 80)
    (set-frame-width (selected-frame) 200)
    (my/frame-recenter))
  (unbind-key (kbd "C-x C-z"))  ;; Unbind suspend frame
  (unbind-key (kbd "C-z"))  ;; Unbind suspend frame
  (global-set-key (kbd "C-c e") 'open-init-file)
  (setq custom-file "~/.emacs.d/custom.el")
  :mode
  ("\\.tsx\\'" . tsx-ts-mode)
  ("\\.ts\\'" . typescript-ts-mode)
  :init
  (defun open-init-file ()
    "Open the init file"
    (interactive)
    (find-file "~/.emacs.d/init.el"))
  (setenv "PATH" (concat "/Library/TeX/texbin/:" (getenv "PATH"))))

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
  :bind	      ("C-c r" . 'vertico-repeat))

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
  ("C-c C-s" . consult-line)
  ("C-x b" . consult-buffer)
  ("C-x ," . consult-recent-file)
  ("M-y" . consult-yank-pop))

(use-package embark-consult
  :after embark)

(use-package orderless
  :ensure (:tag "1.1")
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :ensure (:tag "1.5")
  :init
  (global-corfu-mode 1)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 1)
  :config
  (corfu-echo-mode 1)
  (corfu-history-mode 1))

(use-package doom-modeline
  :ensure (:tag "v4.1.0")
  :config
  (doom-modeline-mode 1))

(use-package all-the-icons
  :ensure (:tag "5.0.0"))

(use-package nerd-icons-corfu
  :ensure (:tag "v0.3.0")
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package dired
  :ensure nil
  :config
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package vterm
  :bind
  ("<f8>" . vterm))

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

  (add-to-list 'project-switch-commands '(sergio/project-vterm "Vterm" "t"))
  (add-to-list 'project-switch-commands '(sergio/project-magit "Magit" "m"))
  (add-to-list 'project-switch-commands '(sergio/project-magit "Modify .dir-locals.el" "v"))
  (define-key project-prefix-map (kbd "t") 'sergio/project-vterm)
  (define-key project-prefix-map (kbd "m") 'sergio/project-magit)
  (define-key project-prefix-map (kbd "v") 'sergio/modify-dir-locals))

(use-package eglot
  :ensure nil
  :hook
  ((python-base-mode terraform-mode) . eglot-ensure)
  :config
  (setq eglot-events-buffer-size 0)
  (setq gc-cons-threshold 1000000000
	read-process-output-max (* 1024 1024))
  (setq-default eglot-workspace-configuration
                '(:pylsp (:plugins (
                                    :flake8 (:enabled :json-false)
                                    :pycodestyle (:enabled :json-false)
                                    :pyflakes (:enabled :json-false)
                                    :mccabe (:enabled :json-false)
                                    :mypy (:enabled :json-false)
                                    :ruff (:enabled t
						    :formatEnabled t
						    :format ["I"]
						    :lineLength 160
						    :targetVersion "py311")
                                    :isort (:enabled t)
				    :rope_autoimport (:enabled t) ;; (Slow AF)
                                    )
                                   :configurationSources ["flake8"])
                         :terraform-ls (:prefillRequiredFields t)))
  (set-face-attribute 'eglot-diagnostic-tag-unnecessary-face nil :underline t :slant 'italic)
  (define-key eglot-mode-map (kbd "C-c l r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c l a") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c l =") 'eglot-format-buffer)
  (define-key eglot-mode-map (kbd "C-c l e") 'flymake-show-buffer-diagnostics))

(use-package python
  :ensure nil
  :config
  ;; (defun sergio/format-buffer-on-save ()
    ;; (add-hook 'before-save-hook 'eglot-format-buffer nil t))
  ;; (add-hook 'python-base-mode-hook 'sergio/format-buffer-on-save)
  :mode
  ("\\.py\\'" . python-ts-mode))

(use-package pyvenv
  :config
  (pyvenv-mode 1)
  (pyvenv-tracking-mode 1))

(use-package terraform-mode)

(use-package cape
  :config
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
  ("C-c g" . magit-status))

(use-package yaml-ts-mode
  :ensure nil
  :mode
  ("\\.yml\\'" . yaml-ts-mode))

(use-package direnv
  :config
  (direnv-mode 1))

(use-package code-cells)

(use-package move-text
  :config
  (move-text-default-bindings))

(use-package iedit)

(use-package git-gutter
  :config
  (global-git-gutter-mode 1))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package json-mode
  :mode
  ("\\.json\\'" . json-mode))

(use-package denote
  :bind
  ("C-c C-n" . denote-open-or-create)
  ("C-c n" . denote-open-or-create)
  :custom
  (denote-directory (if (eq system-type 'darwin) "~/Library/CloudStorage/GoogleDrive-sergiolib@gmail.com/My Drive/Notes/" "~/Documents/Notes/")))

(use-package org
  :ensure nil
  :bind
  ("C-c a" . org-agenda-list)
  ("C-c c" . org-capture)
  ("C-c s" . org-store-link)
  :custom
  (org-indent-indentation-per-level 1)
  (org-agenda-files (if (eq system-type 'gnu/linux)
			(append
			 (f-files "~/Documents/Notes" #'(lambda (f) (s-ends-with? ".org" f)) t)
			 '("~/Insync/sergiolib@gmail.com/Google Drive/Agenda.org"))
		      '("~/Documents/agenda.org")))
  (org-default-notes-file "~/Insync/sergiolib@gmail.com/Google Drive/CapturedTasks.org")
  :hook
  (org-mode . org-indent-mode)
  :config
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sql" . "src sql"))
  (add-hook
   'org-mode-hook
   (lambda ()
     (setq-local electric-pair-inhibit-predicate
		 `(lambda (c)
                    (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
  (require 'ob-sql)
  (org-babel-do-load-languages
   'org-babel-load-languages '((emacs-lisp . t)
			       (sql . t))))

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

(use-package prettier-js
  :hook
  (tsx-ts-mode . prettier-js-mode))

(use-package docker
  :bind
  ("C-c C-d d" . docker)
  ("C-c C-d c" . docker-compose)
  ("C-c C-d C-d" . docker)
  ("C-c C-d C-c" . docker-compose)
  :config
  (setq docker-compose-command "docker compose"))

(use-package rainbow-mode
  :hook
  (prog-mode . rainbow-mode))

(use-package markdown-mode)

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package expand-region
  :bind
  ("C-'" . 'er/expand-region))

(use-package dape
  :after eglot
  :ensure (:tag "0.14.0")
  ;; :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setq dape-key-prefix "\C-x\C-a")

  ;; :hook
  ;; Save breakpoints on quit
  ;; ((kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  ;;  (after-init . dape-breakpoint-load))

  ;; :init
  ;; To use window configuration like gud (gdb-mi)
  ;; (setq dape-buffer-window-arrangement 'gud)

  :config
  ;; Info buffers to the right
  ;; (setq dape-buffer-window-arrangement 'right)

  ;; Global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode)

  ;; Pulse source line (performance hit)
  ;; (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

  ;; To not display info and/or buffers on startup
  ;; (remove-hook 'dape-start-hook 'dape-info)
  ;; (remove-hook 'dape-start-hook 'dape-repl)

  ;; To display info and/or repl buffers on stopped
  ;; (add-hook 'dape-stopped-hook 'dape-info)
  ;; (add-hook 'dape-stopped-hook 'dape-repl)

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-hook 'kill-buffer)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Projectile users
  ;; (setq dape-cwd-fn 'projectile-project-root)
  )
