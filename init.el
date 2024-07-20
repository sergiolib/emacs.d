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
  (setq ring-bell-function #'ignore)
  (tool-bar-mode -1)
  (menu-bar-mode 1)
  (scroll-bar-mode -1)
  (setq modus-themes-mode-line '(accented borderless 1.0))
  (load-theme 'modus-operandi)
  (set-face-attribute 'default nil :height 140 :family "JetBrains Mono")
  (auto-save-visited-mode 1)
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
  (add-hook 'compilation-filter-hook 'ansi-osc-compilation-filter))

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
  ("C-x C-b" . consult-buffer)
  ("C-x ," . consult-recent-file))

(use-package embark-consult)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :init
  (global-corfu-mode 1)
  :bind (:map corfu-map
	      ("RET" . nil)))

(use-package doom-modeline
  :config
  (doom-modeline-mode 1))

(use-package all-the-icons)

(use-package nerd-icons-corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package vterm)

(use-package treemacs
  :config
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
                                      )
                                     :configurationSources ["flake8"])
                           :terraform-ls (:prefillRequiredFields t)
                           :typescript-language-server (:typescript (:format (:indentSize 2)))))
  (set-face-attribute 'eglot-diagnostic-tag-unnecessary-face nil :underline t :slant 'italic)
  :bind (:map eglot-mode-map
	      ("C-c l r" . eglot-rename)
	      ("C-c l a" . eglot-code-actions)))

(use-package python
  :ensure nil
  :config
  (add-hook 'python-mode-hook 'eglot-ensure)
  (defun add-eglot-format-before-save ()
    "Add format on before save hook from eglot"
    (add-hook 'before-save-hook #'(lambda () (when eglot--managed-mode (eglot-format-buffer))) nil t))
  (add-hook 'python-mode-hook 'add-eglot-format-before-save))

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
  :custom
  (denote-directory "~/Documents/Notes/"))

(use-package org
  :ensure nil
  :bind
  ("C-c a" . org-agenda-list)
  ("C-c c" . org-capture)
  :custom
  (org-indent-indentation-per-level 1)
  (org-agenda-files (append (f-files "~/Documents/Notes" #'(lambda (f) (s-ends-with? ".org" f)) t) '("~/Documents/Agenda.org")))
  :hook
  (org-mode . org-indent-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(eglot))
 '(safe-local-variable-values
   '((c-default-style . "k&r")
     (eval setenv "PYTHONPATH" "~/Library/Caches/pypoetry/virtualenvs/anonymizer-A6bZtkw6-py3.12/lib/python3.12/site-packages/")
     (eval setenv "PYTHONPATH" "~/Library/Caches/pypoetry/virtualenvs/")
     (eval setenv "PYTHONPATH" "~/Library/Caches/pypoetry/virtualenvs/non-package-mode-L3eHqBKk-py3.12/lib/python3.12/site-packages/"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
