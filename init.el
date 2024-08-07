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
  :mode
  ("\\.tsx\\'" . tsx-ts-mode)
  ("\\.ts\\'" . typescript-ts-mode))

(use-package ef-themes
  :config
  (load-theme 'ef-maris-light t))

(use-package vertico
  :config
  (vertico-mode 1)
  :custom
  (vertico-cycle t))

(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package embark
  :custom
  (embark-quit-after-action nil)
  :bind
  ("C-." . embark-act))

(use-package consult
  :bind
  ("C-c C-s" . consult-line)
  ("C-x b" . consult-buffer)
  ("C-x ," . consult-recent-file))

(use-package embark-consult)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :init
  (global-corfu-mode 1)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 3))

(use-package doom-modeline
  :config
  (doom-modeline-mode 1))

(use-package all-the-icons)

(use-package nerd-icons-corfu
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
  :config
  (global-set-key (kbd "<f8>") 'vterm))

(use-package treemacs
  :config
  (treemacs)
  (treemacs-project-follow-mode 1))

(use-package eglot
  :ensure nil
  :config
  (setq eglot-events-buffer-size 0)
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
				    ;;:rope_autoimport (:enabled t) ;; (Slow AF)
                                    )
                                   :configurationSources ["flake8"])
                         :terraform-ls (:prefillRequiredFields t)))
  (set-face-attribute 'eglot-diagnostic-tag-unnecessary-face nil :underline t :slant 'italic)
  (define-key eglot-mode-map (kbd "C-c l r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c l a") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c l =") 'eglot-format-buffer))

(use-package python
  :ensure nil
  :config
  (add-hook 'python-base-mode-hook 'eglot-ensure)
  (defun sergio/format-buffer-on-save ()
    (add-hook 'before-save-hook 'eglot-format-buffer nil t))
  (add-hook 'python-base-mode-hook 'sergio/format-buffer-on-save)
  :mode
  ("\\.py\\'" . python-ts-mode))

(use-package pyvenv
  :config
  (pyvenv-mode 1)
  (pyvenv-tracking-mode 1))

(use-package terraform-mode
  :hook
  (terraform-mode . eglot-ensure))

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
  (add-hook 'python-mode-hook #'add-cape-file-to-python-capfs)
  (add-hook 'lisp-data-mode-hook #'add-cape-file-to-lisp-data-capfs)
  (add-hook 'terraform-mode-hook #'add-cape-file-to-tf-capfs))

(use-package which-key
  :config
  (which-key-mode 1))

(use-package emacs
  :ensure nil
  :init
  (defun open-init-file ()
    "Open the init file"
    (interactive)
    (find-file "~/.emacs.d/init.el"))
  :bind
  ("C-c e" . 'open-init-file))

(use-package dockerfile-mode)

(use-package magit
  :ensure (:tag "v3.3.0")
  :bind
  ("C-c g" . magit-status))

(use-package yaml-mode)

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
(use-package avy
  :bind ("C-c C-SPC" . avy-goto-char))

(use-package combobulate
  :ensure (:type git :host github :repo "mickeynp/combobulate")
  :init
  (setq combobulate-key-prefix "C-c o")
  :hook
  ((python-ts-mode . combobulate-mode)
   (js-ts-mode . combobulate-mode)
   (go-mode . go-ts-mode)
   (html-ts-mode . combobulate-mode)
   (css-ts-mode . combobulate-mode)
   (yaml-ts-mode . combobulate-mode)
   (typescript-ts-mode . combobulate-mode)
   (json-ts-mode . combobulate-mode)
   (tsx-ts-mode . combobulate-mode)))

(use-package editorconfig
  :config
  (add-to-list 'editorconfig-indentation-alist '(tsx-ts-mode . typescript-ts-mode-indent-offset))
  (add-hook 'tsx-ts-mode-hook #'editorconfig-apply)
  :hook (tsx-ts-mode . editorconfig-mode))

(use-package prettier-js
  :hook
  (tsx-ts-mode . prettier-js-mode))

(use-package docker
  :config
  (setq docker-compose-command "docker compose")
  (setq docker-run-async-with-buffer-function)
  :bind
  ("C-c d" . docker))

(use-package rainbow-mode
  :hook
  (prog-mode . rainbow-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("6ccb6eb66c70661934a94f395d755a84f3306732271c55d41a501757e4c39fcb" default))
 '(package-selected-packages '(eglot))
 '(safe-local-variable-values
   '((eval let
	   ((buffer-name "api"))
	   (when
	       (not
		(get-buffer-process buffer-name))
	     (async-shell-command
	      (concat "docker compose up db -d && " "cd api/ && " "poetry run aerich upgrade && " "poetry run python scripts/create_user.py -u admin -p admin -n Admin -e admin@substorm.ai -s &&" "poetry run uvicorn --reload --host 0.0.0.0 anonymizer.main:app")
	      buffer-name)))
     (let
	 ((buffer-name "db"))
       (when
	   (not
	    (get-buffer-process buffer-name))
	 (async-shell-command
	  (concat "docker compose up db -d && " "cd api/scripts && " "poetry run python create_user.py -u admin -p admin -n Admin -e admin@substorm.ai -s")
	  buffer-name)))
     (let
	 ((buffer-name "db"))
       (when
	   (not
	    (get-buffer-process buffer-name))
	 (async-shell-command "docker compose up db -d && cd api/scripts && poetry run python create_user.py -u admin -p admin -n Admin -e admin@substorm.ai -s" buffer-name)))
     (eval let
	   ((buffer-name "ui"))
	   (when
	       (not
		(get-buffer-process buffer-name))
	     (async-shell-command "cd ui/ && npm start" buffer-name)))
     (eval let
	   ((buffer-name "api"))
	   (when
	       (not
		(get-buffer-process buffer-name))
	     (async-shell-command "cd api/ && poetry run uvicorn --host 0.0.0.0 --port 9797 --reload anonymizer.main:app" buffer-name)))
     (eval async-shell-command "echo \"hello!\"")
     (eval async-command-shell "echo \"hello!\"")
     (eval setenv "PYTHONPATH" "/Users/sliberman/Documents/src/Substorm.Anonymizer/api/.venv/lib/python3.12/site-packages/")
     (c-default-style . "k&r")
     (eval setenv "PYTHONPATH" "~/Library/Caches/pypoetry/virtualenvs/anonymizer-A6bZtkw6-py3.12/lib/python3.12/site-packages/")
     (eval setenv "PYTHONPATH" "~/Library/Caches/pypoetry/virtualenvs/")
     (eval setenv "PYTHONPATH" "~/Library/Caches/pypoetry/virtualenvs/non-package-mode-L3eHqBKk-py3.12/lib/python3.12/site-packages/"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
