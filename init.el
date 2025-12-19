;; Right option key on the Mac is a keyboard modifier
(setq ns-right-option-modifier nil)

;; Load elpaca package manager
(load "~/.emacs.d/elpaca.el")

;; Install and configure vertico
(use-package vertico
  :ensure t
  :config
  (vertico-mode 1))

;; Install and configure orderless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

;; Configure savehist
(use-package savehist
  :config
  (savehist-mode 1))

;; Configure recentf
(use-package recentf
  :config
  (recentf-mode 1))

;; Install and configure consult
(use-package consult
  :ensure t
  :bind
  ("C-c s" . consult-line)
  ("C-x b" . consult-buffer)
  ("M-y" . consult-yank-pop)
  ("C-x ," . consult-recent-file))

;; Install and configure embark
(use-package embark
  :ensure t
  :config
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode 1)
  (global-set-key (kbd "C-.") 'embark-act))

;; Install and configure embark consult
(use-package embark-consult
  :ensure t)

;; Install and configure denote
(use-package denote
  :ensure t
  :bind
  ("C-c n" . denote-open-or-create)
  :config
  (setq denote-directory '("~/Library/CloudStorage/GoogleDrive-sergiolib@gmail.com/My Drive/Notes/")))

;; Configure and install magit
(use-package transient
  :ensure (:pin "0.10"))

(use-package magit
  :ensure t
  :bind
  ("C-c g" . magit-status))

;; Configure project.el
(use-package project
  :config
  (add-to-list 'project-vc-extra-root-markers "pyproject.toml")
  (defun sergio/consult-ripgrep ()
    (interactive)
    (consult-ripgrep (project-root (project-current))))
  (define-key project-prefix-map (kbd "g") 'sergio/consult-ripgrep)
  (add-to-list 'project-switch-commands '(project-find-regexp "Find regexp" (kbd "g"))))

;; Paredit
(use-package paredit
  :ensure t)

;; Configure and install which-key
(use-package which-key
  :config
  (which-key-mode 1))

;; Configure emacs frame
(use-package emacs
  :config
  (menu-bar-mode 1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (set-face-attribute 'default nil :height 130 :family "JetBrains Mono"))

;; Configure all the icons and nerd icons
(use-package all-the-icons
  :ensure t)

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-corfu
  :after (nerd-icons corfu)
  :ensure t
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Nerd icons in dired
(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; Configure doom modeline
(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-modal-icon t)
  (doom-modeline-buffer-encoding nil)
  )

;; Configure modus themes
(use-package emacs
  :config
  (setq modus-themes-common-palette-overrides
	'((border-mode-line-active bg-mode-line-active)
          (border-mode-line-inactive bg-mode-line-inactive)
	  (bg-mode-line-active bg-cyan-subtle)
          (fg-mode-line-active fg-main)))
  (load-theme 'modus-operandi))

;; Configure eglot
(use-package eglot
  :init
  (add-hook 'python-base-mode-hook 'eglot-ensure)
  (add-hook 'go-ts-mode-hook 'eglot-ensure)
  :bind (:map eglot-mode-map
	      ("C-c r" . eglot-rename)
	      ("C-c a" . eglot-code-actions)
	      ("C-c h" . eldoc))
  :config
  (define-key eglot-mode-map (kbd "<mouse-8>") 'xref-go-back)
  (define-key eglot-mode-map (kbd "<drag-mouse-8>") 'xref-go-back)
  (define-key eglot-mode-map (kbd "<mouse-9>") 'xref-go-forward)
  (define-key eglot-mode-map (kbd "<drag-mouse-9>") 'xref-go-forward))

;; Install and configure corfu
(use-package corfu
  :ensure t
  :config
  (global-corfu-mode 1)
  (corfu-history-mode 1)
  (corfu-popupinfo-mode 1)
  (setq corfu-popupinfo-delay '(0.1 . 0.1))
  (corfu-echo-mode -1)
  (setq corfu-auto t)
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
	 (consult-completion-in-region beg end table pred)))))
  (keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer))

;; Install and configure marginalia
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

;; Install and configure apheleia
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode 1)
  :hook
  (python-base-mode . (lambda () (setq apheleia-formatter '(ruff ruff-isort)))))

;; Install and configure dape
(use-package dape
  :ensure t
  :hook
  ;; Save breakpoints on quit
  (kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  (after-init . dape-breakpoint-load)
  :custom
  ;; Turn on global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode +1))

;; For a more ergonomic Emacs and `dape' experience
(use-package repeat
  :custom
  (repeat-mode +1))

;; Left and right side windows occupy full frame height (dape mode)
(use-package emacs
  :custom
  (window-sides-vertical t))

;; Configure vterm
(use-package vterm
  :ensure t
  :bind
  ("<f8>" . vterm))

;; Configure python
(use-package python
  :config
  (add-hook 'python-base-mode-hook #'(lambda () (setq-local fill-column 120)))
  (add-hook 'python-base-mode 'which-function-mode)
  :mode
  ("\\.py\\'" . python-ts-mode))

;; Handy global keymaps for emacs
(use-package emacs
  :init
  ;; C-c e opens init file
  (global-set-key (kbd "C-c e") #'(lambda () (interactive) (find-file "~/.emacs.d/init.el"))))

;; Org mode
(use-package org
  :bind
  ("C-c C-a" . org-agenda)
  :config
  (let ((org-notes (f-glob "*.org" "~/Library/CloudStorage/GoogleDrive-sergiolib@gmail.com/My Drive/Notes/")))
    (setq org-agenda-files (append org-notes
				   (list "~/Library/CloudStorage/GoogleDrive-sergiolib@gmail.com/My Drive/Agenda.org"))))
  (add-hook 'org-mode-hook #'(lambda () (electric-pair-local-mode -1)))
  (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
							   (shell . t)
							   (verb . t)))
  :custom
  (org-confirm-babel-evaluate nil))

(use-package org-tempo
  :after org
  :config
  (add-to-list 'org-structure-template-alist '("sql" . "src sql"))
  (add-to-list 'org-structure-template-alist '("verb" . "src verb"))
  (add-to-list 'org-structure-template-alist '("sh" . "src bash"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("js" . "src javascript"))
  )

;; Extras for eglot
(use-package yasnippet
  :ensure t)

(use-package yasnippet-snippets
  :ensure t)

(use-package markdown-mode
  :ensure t)

;; Generics
(use-package emacs
  :config
  ;; Initial message
  (setq inhibit-startup-message t)
  (setq initial-scratch-message nil)
  ;; Auto save and revert
  (auto-save-visited-mode -1)
  (auto-revert-mode 1)
  ;; Monday is week start
  (setq calendar-week-start-day 1)
  ;; Parens highlight expressions
  (setq show-paren-style 'expression)
  ;; Global line numbers
  (setq
   display-line-numbers-type 'relative
   display-line-numbers-widen t
   excluded-hooks-from-display-numbers '(doc-view-mode-hook))
  (mapc (lambda (hook)
	  (add-hook hook #'(lambda () (display-line-numbers-mode 0))))
	excluded-hooks-from-display-numbers)
  (let ((no-line-number-hooks '(vterm-mode-hook help-mode-hook)))
    (dolist (hook no-line-number-hooks)
      (add-hook hook #'(lambda () (display-line-numbers-mode -1)))))
  (global-display-line-numbers-mode 1)
  ;; Parens inserted in pairs
  (electric-pair-mode 1)
  ;; Tab widths
  (setq tab-width 4)
  (add-hook 'typescript-ts-base-mode-hook #'(lambda () (setq tab-width 2)))
  ;; Turn off bell
  (setq ring-bell-function #'ignore)
  ;; Improved compilation mode
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
  (add-hook 'compilation-filter-hook 'ansi-osc-compilation-filter)
  ;; Winner mode
  (winner-mode 1)
  ;; Key binds for moving between windows
  (windmove-default-keybindings '(C S))
  ;; Unbind suspend frame
  (unbind-key (kbd "C-x C-z"))
  (unbind-key (kbd "C-z"))
  ;; Custom file
  (setq custom-file "~/.emacs.d/custom.el")
  ;; Ignore case during completion
  (setq completion-ignore-case t
	read-file-name-completion-ignore-case t
	read-buffer-completion-ignore-case t)

  ;; Show column numbers
  (column-number-mode 1)
  ;; Yes -> y, No -> n
  (defalias 'yes-or-no-p 'y-or-n-p))

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

(use-package go-ts-mode
  :mode "\\.go$")

(use-package gptel
  :ensure t
  :config
  (setq
   gptel-model 'gemini-2.5-pro
   gptel-backend (gptel-make-gemini "Cenit"
                   :key "AIzaSyDEdyzBU6-v1ypi6JEPeNs9s6Yrm7aUzg8"
                   :stream t)))

;; Install and configure verb mode
(use-package verb
  :ensure t)

(use-package org
  :after verb
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(use-package track-changes
  :ensure t)

(use-package copilot
  :ensure (:host github
		 :repo "copilot-emacs/copilot.el")
  :hook
  (python-base-mode . copilot-mode)
  :bind (:map copilot-completion-map
	      ("<tab>" . 'copilot-accept-completion)
	      ("TAB" . 'copilot-accept-completion)))

(use-package multiple-cursors
  :ensure t
  :bind (("M-¿" . mc/mark-next-like-this)
	 ("M-¡" . mc/mark-previous-like-this)))

(use-package expand-region
  :ensure t
  :bind ("M-+" . er/expand-region))
