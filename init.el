(defvar native-comp-deferred-compilation-deny-list nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package gcmh
  :straight (gcmh :host github :repo "emacsmirror/gcmh")
  :init
  (gcmh-mode 1))

(setq straight-use-package-by-default t)

(straight-use-package 'use-package)

(auto-save-visited-mode -1)
(setq auto-save-visited-interval 60)

(setq calendar-week-start-day 1)
(setq native-comp-async-report-warnings-errors nil)

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-p") nil)
  (define-key evil-insert-state-map (kbd "C-n") nil)
  (evil-local-set-key 'normal (kbd "g r") 'revert-buffer)
  (global-set-key (kbd "C-M-u") 'universal-argument)
  :custom
  (evil-undo-system 'undo-redo)
  (evil-want-integration t)
  (evil-want-C-u-scroll t)
  (evil-want-C-d-scroll t)
  (evil-want-C-i-jump t)
  (evil-want-Y-yank-to-eol t)
  (evil-want-fine-undo t)
  (evil-esc-delay 0.001)
  (evil-auto-indent t)
  (evil-jumps-cross-buffers nil)
  (evil-symbol-word-search t))

(use-package evil-collection
  :init
  (evil-collection-init))

(use-package evil-surround
  :init
  (global-evil-surround-mode 1))

(use-package general
  :init
  (general-create-definer leader
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi t)
  :custom
  (modus-themes-common-palette-overrides '((border-mode-line-active unspecified)
					   (border-mode-line-inactive unspecified)
					   (bg-mode-line-active bg-blue-intense)
					   (fg-mode-line-active fg-main))))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*.el"))
  :init
  (vertico-mode 1)
  (setq vertico-cycle t)
  (unbind-key (kbd "C-m") 'vertico-map)
  (add-hook 'minibuffer-mode-hook #'(lambda () (pixel-scroll-precision-mode -1))))

(use-package vertico-repeat
  :commands (vertico-repeat-save vertico-repeat)
  :straight (vertico :files (:defaults "extensions/*.el"))
  :init
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  :bind
  ("M-R" . #'vertico-repeat))

(use-package vertico-quick
  :commands (vertico-quick-insert vertico-quick-exit)
  :straight (vertico :files (:defaults "extensions/*.el"))
  :init
  (keymap-set vertico-map "M-q" #'vertico-quick-insert)
  (keymap-set vertico-map "C-q" #'vertico-quick-exit))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion))))
  (defun orderless-fast-dispatch (word index total)
    (and (= index 0) (= total 1) (length< word 4)
	 `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

  (orderless-define-completion-style orderless-fast
    (orderless-style-dispatchers '(orderless-fast-dispatch))
    (orderless-matching-styles '(orderless-literal orderless-regexp))))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode 1))

(use-package corfu
  :init
  (global-corfu-mode 1)
  (setq corfu-auto t)
  (setq corfu-cycle t)
  (setq corfu-min-width 3)
  (setq corfu-quit-no-match 'separator)

  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
		  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
  :bind (:map corfu-map
	 ("TAB" . corfu-complete)
	 ([tab] . corfu-complete)
	 ("RET" . corfu-complete)
	 ([ret] . corfu-complete)))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(setq completion-cycle-threshold 3)
(setq tab-always-indent 'complete)

(use-package python
  :mode ("\\.python\\'" . python-mode)
  :init
  (add-hook 'python-mode-hook 'pyvenv-mode)
  (add-hook 'python-mode-hook 'pyvenv-tracking-mode))

(use-package embark
  :commands (embark-act embark-dwim embark-bindings)
  :init
  (dolist (map (list corfu-map
		     minibuffer-mode-map
		     evil-insert-state-map
		     evil-normal-state-map
		     evil-visual-state-map
		     evil-emacs-state-map))
    (define-key map (kbd "C-.") 'embark-act)
    (define-key map (kbd "M-.") 'embark-dwim)
    (define-key map (kbd "C-h B") 'embark-bindings))
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  :custom
  (embark-prompter 'embark-keymap-prompter)
  (embark-quit-after-action '((t . nil)))
  :config
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package cape
  :init
  (defalias 'cape-eglot-dabbrev
    (cape-super-capf (cape-capf-buster #'eglot-completion-at-point) #'cape-dabbrev))
  (add-hook 'eglot-completion-mode-hook #'(lambda () (setq-local completion-at-point-functions '(cape-eglot-dabbrev cape-file))))
  (defalias 'cape-elisp+dabbrev
    (cape-super-capf #'elisp-completion-at-point #'cape-dabbrev))
  (add-hook 'emacs-lisp-mode-hook #'(lambda () (setq-local completion-at-point-functions '(cape-elisp+dabbrev cape-file)))))

(use-package which-key
  :init
  (which-key-mode 1))

(leader
  "e" '(:ignore t :which-key "Emacs configuration")
  "ee" #'(lambda () (interactive) (find-file user-init-file)))

(use-package eshell
  :commands (eshell)
  :config
  (add-hook 'eshell-mode-hook
	    #'(lambda ()
		(setq-local corfu-auto nil)
		(corfu-mode)))
  (defun corfu-send-shell (&rest _)
    "Send completion candidate when inside comint/eshell."
    (cond
     ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
      (eshell-send-input))
     ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
      (comint-send-input))))

  (advice-add #'corfu-insert :after #'corfu-send-shell))

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook
		vterm-mode-hook
		treemacs-mode-hook
		inferior-python-mode-hook
		pdf-view-mode-hook))
  (add-hook mode #'(lambda () (display-line-numbers-mode 0))))

(defun set-window-faces (frame)
  (set-face-attribute 'default nil :family "JetBrains Mono" :height 110 :weight 'semi-light)
  (set-face-attribute 'fixed-pitch nil :family "JetBrains Mono" :height 110)
  (set-face-attribute 'variable-pitch nil :family "Noto Serif" :height 120)
  (remove-hook 'after-make-frame-functions 'set-window-faces))
(if (and (boundp 'server-process)
	 (processp server-process)
	 (server-running-p))
    (add-hook 'after-make-frame-functions #'set-window-faces)
  (set-window-faces nil))

(leader
  "t" '(:ignore t :which-key "Tabs")
  "tr" 'tab-rename
  "th" 'tab-previous
  "tl" 'tab-next
  "tn" 'tab-new
  "tk" 'tab-close)

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-modal-icon nil)
  (setq doom-modeline-env-version t))

(use-package all-the-icons)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(show-paren-mode 1)
(setq show-paren-style 'expression)
(setq show-paren-when-point-inside-paren nil)

(add-hook 'emacs-lisp-mode-hook 'electric-pair-local-mode)
(add-hook 'lisp-data-mode-hook 'electric-pair-local-mode)

(setq initial-scratch-message "")

(column-number-mode 1)

(setq global-auto-revert-non-file-buffers 1)
(global-auto-revert-mode 1)
(setq-default global-auto-revert-ignore-modes '(vterm-mode Buffer-menu-mode))

(setq display-time-default-load-average nil)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-format "%H:%M %d-%m-%Y")
(display-time-mode)

(display-battery-mode 1)

(defun sergio/consult-ripgrep-on-project ()
  (interactive)
  (let ((dir (project-root (project-current))))
    (consult-ripgrep dir)))

(use-package consult
  :bind (
	 ("C-x f" . consult-find)
	 ("C-s" . consult-line)
	 (:map minibuffer-local-map
	  ("C-r" . consult-history)
	  ("C-x b" . consult-buffer))
	 (:map project-prefix-map
	  ("r" . sergio/consult-ripgrep-on-project)))
  :general (leader
	     "et" 'consult-theme
	     "," 'consult-recent-file
	     "y" 'consult-yank-from-kill-ring
	     "o" 'consult-outline)
  :config
  (consult-customize consult-theme :preview-key '(:debounce 0.5 any)))

(winner-mode 1)

(use-package magit
  :general
  (leader
    "g" '(:ignore t :which-key "git")
    "gg" 'magit-status))

(use-package vterm
  :general
  (leader
    "x" '(:ignore t :which-key "Terminals")
    "xx" 'vterm
    "xv" 'vterm-other-window))

(leader
  "p" '(:keymap project-prefix-map :package project :which-key "project"))

(setq inhibit-startup-message t)
(setq system-time-locale "C")
(tooltip-mode -1)
(setq visual-bell t)

(setq enable-local-eval t)

(put 'python-shell-extra-pythonpaths 'safe-local-variable (lambda (_) t))

(use-package cmake-mode
  :mode ("\\.cmake\\'" . cmake-mode))

(use-package dockerfile-mode
  :mode ("\\.dockerfile\\'" . dockerfile-mode))

;; Unbind suspend frame hotkey
(unbind-key (kbd "C-x C-z") global-map)

(use-package helpful
  :bind
  ("C-h f" . helpful-function)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h ." . helpful-at-point))

(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))

(use-package docker
  :init
  (setq docker-run-async-with-buffer-function 'docker-run-async-with-buffer-vterm)
  (setq docker-compose-command "docker compose")
  :general
  (leader
    "d" '(:ignore t :which-key "Containers")
    "dd" 'docker
    "dc" 'docker-compose))

(use-package evil-nerd-commenter
  :bind
  ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package evil-multiedit
  :config
  (evil-multiedit-default-keybinds)
  (unbind-key (kbd "<insert-state> RET") 'evil-multiedit-mode-map)
  :custom
  (evil-multiedit-use-symbols nil))

(setenv "WORKON_HOME" "/home/sliberman/envs/")
(use-package pyvenv
  :commands (pyvenv-mode pyvenv-tracking-mode))

(leader "er" 'revert-buffer)

(recentf-mode 1)

(use-package avy
  :commands (avy-goto-char)
  :init
  (define-key evil-normal-state-map (kbd "g c") 'avy-goto-char))

(global-set-key (kbd "C-x C-k") 'kill-this-buffer)

(use-package org
  :config
  (defun sergio/org-font-setup ()
    ;; Set faces for heading levels
    (dolist (face '((org-level-1 . 2.0)
		    (org-level-2 . 1.3)
		    (org-level-3 . 1.1)
		    (org-level-4 . 1.1)
		    (org-level-5 . 1.0)
		    (org-level-6 . 1.0)
		    (org-level-7 . 1.0)
		    (org-level-8 . 1.0)))
      (set-face-attribute (car face) nil :height (cdr face)))
    ;; Ensure that anything that should be fixed-pitch in Org files appears that way
    (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
    (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face variable-pitch))
    (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
    (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
    (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)
    (setq org-todo-keywords
	  (quote ((sequence "TODO(t)" "|" "ABANDONED(b)" "DONE(d)"))))
    (setq org-log-done t))
  :custom
  (org-ellipsis " ▼")
  (org-latex-pdf-process (list "latexmk -f -pdf %f"))
  (org-confirm-babel-evaluate nil)
  (org-image-actual-width nil)
  (org-latex-caption-above nil)
  (org-src-window-setup 'current-window)
  (org-edit-src-content-indentation 0)
  ;; (org-export-in-background nil)
  (org-odt-preferred-output-format "docx")
  :hook
  (org-mode . url-handler-mode)
  (org-mode . visual-line-mode)
  (org-mode . (lambda () (variable-pitch-mode 1)))
  (org-mode . org-indent-mode)
  (org-mode . sergio/org-font-setup)
  (org-mode . (lambda () (setq-local evil-auto-indent nil)))
  :config
  (setq org-indent-indentation-per-level 2)
  (defun evil-org-insert-state-in-edit-buffer (fun &rest args)
    "Bind `evil-default-state' to `insert' before calling FUN with ARGS."
    (let ((evil-default-state 'insert)
          ;; Force insert state
          evil-emacs-state-modes
          evil-normal-state-modes
          evil-motion-state-modes
          evil-visual-state-modes
          evil-operator-state-modes
          evil-replace-state-modes)
      (apply fun args)
      (evil-refresh-cursor)))

  (advice-add 'org-babel-do-key-sequence-in-edit-buffer
              :around #'evil-org-insert-state-in-edit-buffer)

  (require 'org-tempo)

  (dolist (template '(("sh" . "src shell")
		      ("el" . "src emacs-lisp")
		      ("py" . "src python")
		      ("ja" . "src java")
		      ("sql" . "src sql")
		      ("cc" . "src C")))
    (add-to-list 'org-structure-template-alist template))

  ;; Babel languages.
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (latex . t)
     (shell . t)
     (C . t)
     (sql . t)
     (java . t)
     (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes)
  :mode ("\\.org\\'" . org-mode))

(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode)
  :config
  ;; (defface bullets '((default . (:family "Font Awesome 6 Free"))) "Face for the bullets in org mode")
  ;; (setq org-bullets-face-name 'bullets)
  (setq org-bullets-bullet-list '("◉" "○" "✸" "✿")))

(defun sergio/visual-fill ()
  (setq visual-fill-column-width 120
	visual-fill-column-center-text t
	visual-fill-column-fringes-outside-margins nil)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook ((org-mode . sergio/visual-fill)))

(use-package denote
  :init
  (setq denote-directory "~/Documents/Notes")
  :general
  (leader
    "n" '(:ignore t :which-key "Notes")
    "nf" 'denote-open-or-create
    "nn" 'denote
    "nl" 'denote-link-or-create))
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      backup-directory-alist '(("." . "~/.emacs.d/backups")))

(use-package flyspell
  :hook
  (org-mode . flyspell-mode))

(use-package treemacs
  :general
  (leader "eb" 'treemacs)
  :config
  (setq treemacs-follow-mode t))

(use-package eglot
  :config
  (add-hook 'before-save-hook (lambda () (interactive) (when (eglot-managed-p) (eglot-format-buffer))))
  (set-face-attribute 'eglot-highlight-symbol-face nil :inherit 'bold :underline t)
  :hook
  (python-mode . eglot-ensure)
  :general (leader
	     "l" '(:ignore t :which-key "LSP")
	     "lr" 'eglot-rename
	     "l=" 'eglot-format-buffer
	     "lg" '(:ignore t :which-key "Go to")
	     "le" 'flymake-show-buffer-diagnostics))

(load-file "~/.emacs.d/fix_keywords_align.el")

;; Indent the buffer in emacs-lisp mode and lisp-data mode
(add-hook 'before-save-hook (lambda ()
			      (interactive)
			      (when (or (eq major-mode 'emacs-lisp-mode) (eq major-mode 'lisp-data-mode)
					(save-excursion
					  (indent-region (point-min) (point-max)))))))

(add-hook 'emacs-lisp-mode-hook 'flymake-mode)
