;; Added By Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Package
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))


;; warning level
(setq warning-minimum-level :emergency)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(use-package bind-key
  :ensure t)

;; Native compile
(setq package-native-compile t)
(use-package auto-async-byte-compile
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))

(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

;;;;; Encoding ;;;;;
(set-language-environment 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;;;; Environment ;;;;;
(setq auto-save-default nil
	  create-lockfiles nil
	  delete-auto-save-files t
	  make-backup-files nil
	  ;; suppress bell
	  ring-bell-function 'ignore
	  ;; always insert a newline at the end
	  require-final-newline t)

(scroll-bar-mode -1)
(tool-bar-mode -1)

;; expand region
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-popup-type 'minibuffer))

;; window size
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; window manager
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-x o") 'ace-window))

;; presentation
(use-package presentation
  :ensure t)

;; smart move
(use-package mwim
  :ensure t
  :bind
  (("C-a" . mwim-beginning-of-code-or-line)
   ("C-e" . mwim-end-of-code-or-line)))


;; comment-dwim
(use-package comment-dwim-2
  :ensure t
  :config
  (global-set-key (kbd "M-;") 'comment-dwim-2))

;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(setopt use-short-answers t)
(setq-default cursor-type 'bar)
(setq default-directory "~/"
	  command-line-default-directory "~/"
	  inhibit-startup-message t
	  kill-whole-line t
	  max-specpdl-size 10000)
(savehist-mode 1)
(save-place-mode 1)

(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package hungry-delete
  :ensure t
  :diminish
  :hook (after-init . global-hungry-delete-mode))

(use-package disable-mouse
  :ensure t
  :diminish disable-mouse-mode
  :init
  (setq disable-mouse-wheel-events nil)
  :config
  (global-disable-mouse-mode))

;; highlight keyword
(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
		'(("TODO" . "#cc9393")
		  ("FIXME" . "#cc9393")
		  ("DEBUG" . "#A020F0"))))

;; hightlight symbol
(use-package highlight-symbol
  :ensure t
  :config
  (setq highlight-symbol-idle-delay 0.5)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (add-hook 'prog-mode-hook 'highlight-symbol-nav-mode))

;; quelpa use-package
(use-package quelpa-use-package
  :ensure t)


;;;;; nerd-icon ;;;;;
;; Nerd font is required
(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "MesloLGL Nerd Font Mono"))


;;;;; Dashboard ;;;;;
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-init-info t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t))


;;;;; Dired related packages;;;;;
(use-package dired+
  :quelpa (dired+ :fetcher url :url "https://www.emacswiki.org/emacs/download/dired+.el")
  :defer 1
  :init
  (setq diredp-hide-details-initially-flag nil
		diredp-hide-details-propagate-flag nil)
  :config
  (diredp-toggle-find-file-reuse-dir 1))

(use-package dired-subtree
  :ensure t
  :config
  (bind-keys :map dired-mode-map
			 ("i" . dired-subtree-insert)
			 (";" . dired-subtree-remove)
			 ("C-, C-p" . dired-subtree-up)
			 ("C-, C-n" . dired-subtree-down)))


;;;;; Dimmer ;;;;;
(use-package dimmer
  :ensure t
  :custom
  (dimmer-fraction 0.3)
  (dimmer-watch-frame-focus-events nil)
  (dimmer-buffer-exclusion-regexps
   '("^\\*Minibuf-[0-9]+\\*" "^.\\*which-key\\*$"
	 "^*Messages*" "*LV*" "transient"))
  :config
  (dimmer-configure-magit)
  (dimmer-configure-posframe)
  (dimmer-configure-which-key)
  (dimmer-mode t)
  ;; make it compatible to corfu
  ;; https://github.com/gonewest818/dimmer.el/issues/62
  (defun advise-dimmer-config-change-handler ()
    "Advise to only force process if no predicate is truthy."
    (let ((ignore (cl-some (lambda (f) (and (fboundp f) (funcall f)))
                           dimmer-prevent-dimming-predicates)))
      (unless ignore
        (when (fboundp 'dimmer-process-all)
          (dimmer-process-all t)))))
  
  (defun corfu-frame-p ()
	"Check if the buffer is a corfu frame buffer."
	(string-match-p "\\` \\*corfu" (buffer-name)))
  
  (defun dimmer-configure-corfu ()
	(add-to-list
     'dimmer-prevent-dimming-predicates
     #'corfu-frame-p))

  (advice-add
   'dimmer-config-change-handler
   :override 'advise-dimmer-config-change-handler)

  (dimmer-configure-corfu))


;;;;; Undo Tree ;;;;;
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history nil))


;;;;; treemacs ;;;;;
(use-package treemacs
  :ensure t
  :bind
  (:map global-map
		("C-q" . treemacs))
  :custom
  (treemacs-follow-after-init t)
  (treemacs-expand-after-init t)
  (treemacs-display-in-side-window t)
  (treemacs-position 'left)
  (treemacs-show-hidden-files t)
  (treemacs-hide-dot-git-directory t)
  (treemacs-filewatch-mode t)
  (treemacs-follow-mode t)
  (treemacs-project-follow-mode t)
  (treemacs-indentation 2)
  (treemacs-event-guide-mode t))


;;;;; tab bar ;;;;;
(tab-bar-mode 1)
;;(global-tab-line-mode)

;;;;; Spell checking ;;;;;
(setq-default ispell-program-name "aspell")
(with-eval-after-load "ispell"
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

(use-package flyspell
  :ensure t
  :diminish flyspell-mode
  :config
  (define-key flyspell-mode-map (kbd "C-;") nil)
  (add-hook 'prog-mode-hook
			'(lambda ()
			   (flyspell-prog-mode))))


;; modus theme
(setq modus-themes-bold-constructs t
	  modus-themes-italic-constructs t
	  modus-themes-mixed-fonts t
	  modus-themes-fringes 'nil
	  modus-themes-region '(bg-only no-extend)
	  modus-themes-subtle-line-numbers t
	  modus-themes-syntax '(faint alt-syntax green-strings)
	  modus-themes-paren-match 'intense
	  modus-themes-hl-line 'accented
	  modus-themes-variable-pitch-ui t
	  modus-themes-prompts '(bold background)
	  modus-themes-mode-line '(moody))
(load-theme 'modus-vivendi)

;; modeline
(use-package moody
  :ensure t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

(use-package minions
  :ensure t
  :diminish minions-mode
  :config
  (minions-mode 1)
  (setq minions-mode-line-lighter "[+]"))


;;;;; yasnippet ;;;;;
(use-package yasnippet
  :ensure t
  :hook
  (prog-mode . yas-minor-mode)
  :bind
  (("C-c y n" . yas-new-snippet)
   ("C-c y v" . yas-visit-snippet-file)
   ("C-c y i" . yas-insert-snippet))
  :config
  (yas-reload-all)
  (setq yas-snippet-dirs
		'("~/.emacs.d/snippets")))


;;;;; tree-sitter ;;;;;
(use-package tree-sitter
  :ensure t
  :config
  (use-package tree-sitter-langs
	:ensure t)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))


;;;;; lsp-mode ;;;;;
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook
  ((rust-mode . (lsp lsp-inlay-hints-mode))
   (go-mode . (lsp lsp-inlay-hints-mode))
   (kotlin-mode . lsp)
   (c++-mode . lsp)
   (lua-mode . lsp))
  :custom
  ;; cc-mode does not work well when following two settings are enabled.
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-indentation nil)
  (lsp-diagnostics-provider :flymake)
  (lsp-headerline-breadcrumb-icons-enable t)
  (lsp-enable-snippet t)
  (lsp-auto-guess-root t)
  (lsp-idle-delay 0.3)
  (lsp-log-io t)
  (lsp-modeline-code-actions-enable nil)
  (lsp-completion-provider :none)
  (lsp-eldoc-render-all t)
  :bind
  (:map lsp-mode-map
		("C-c C-l" . lsp-execute-code-action)
		("C-c r" . lsp-rename))
  :config
  (add-hook 'c++-mode-hook '(lambda() (add-hook 'before-save-hook 'lsp-format-buffer t t))))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode-hook . lsp-ui-mode)
  :custom
  ;; doc
  (lsp-ui-doc-enable nil)
  ;; sideline
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-hover nil)
  :init
  (defun toggle-lsp-ui-sideline ()
	(interactive)
	(if lsp-ui-sideline-show-hover
        (progn
          (setq lsp-ui-sideline-show-hover nil
				lsp-ui-sideline-show-code-actions nil
				lsp-ui-sideline-show-diagnostics nil)
          (message "sideline-hover disabled"))
      (progn
        (setq lsp-ui-sideline-show-hover t
			  lsp-ui-sideline-show-code-actions t
			  lsp-ui-sideline-show-diagnostics t)
        (message "sideline-hover enabled"))))
  (defun toggle-lsp-ui-imenu ()
    (interactive)
	(let ((imenu-buffer (get-buffer lsp-ui-imenu-buffer-name)))
	  (if imenu-buffer
		  (progn
			(lsp-ui-imenu-buffer-mode -1)
			(kill-buffer lsp-ui-imenu-buffer-name)
			(message "lsp-ui-imenu disabled"))
		(progn
		  (lsp-ui-imenu)
		  (message "lsp-ui-imenu enabled")))))
  :bind
  (:map lsp-mode-map
		("C-c C-i" . lsp-ui-peek-find-implementation)
		("M-." . lsp-ui-peek-find-definitions)
		("M-?" . lsp-ui-peek-find-references)
		("C-c i" . toggle-lsp-ui-imenu)
		("C-c C-s" . toggle-lsp-ui-sideline)))


(use-package lsp-treemacs
  :ensure t
  :after lsp-mode)


;;;;; dap ;;;;;
(use-package dap-mode
  :ensure t)


;;;;; Org mode ;;;;;
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key (kbd "C-c a") 'org-agenda)
(use-package toc-org
  :ensure t
  :init
  (add-hook 'org-mode-hook 'toc-org-mode)
  ;; (add-hook 'markdown-mode-hook 'toc-org-mode)
  ;; :config
  ;; (define-key markdown-mode-map (kbd "C-c C-o") 'toc-org-markdown-follow-thing-at-point)
  )


;;;;; Project Root ;;;;;
(use-package projectile
  :ensure t
  :diminish projectile-mode)
(defun my-projectile-project-find-function (dir)
  (let ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))
(projectile-mode t)
(with-eval-after-load 'project
  (add-to-list 'project-find-functions 'my-projectile-project-find-function))



;;;;; Coding Style ;;;;;

;; minimap
(use-package minimap
  :ensure t
  :commands
  (minimap-buffer-name minimap-create-window minimap-kill)
  :diminish
  :custom
  (minimap-major-modes '(prog-mode))
  (minimap-minimum-width 15)
  (minimap-window-location 'right)
  :bind
  (("C-c C-m" . minimap-mode)))

;; Code folding
(use-package origami
  :ensure t
  :diminish
  :hook (after-init . global-origami-mode)
  :bind
  (("C-c o o" . origami-open-node)
   ("C-c o c" . origami-close-node)))

;; line number
(global-display-line-numbers-mode)

;; indentation
(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode))

(use-package highlight-indent-guides
  :ensure t
  :hook
  ((prog-mode yaml-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-auto-enabled nil)
  :custom-face
  (highlight-indent-guides-character-face ((t (:foreground "dimgray"))))
  )

(column-number-mode t)
(electric-pair-mode 1)
(show-paren-mode 1)
(defvar show-paren-delay 0)
(which-function-mode t)

(setq-default tab-width 4)

;; parenthesis color
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (use-package cl-lib
	:ensure t)
  (use-package color
	:ensure t
	:config
	(defun rainbow-delimiters-using-stronger-colors ()
	  (interactive)
	  (cl-loop
	   for index from 1 to rainbow-delimiters-max-face-count
	   do
	   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
		 (cl-callf color-saturate-name (face-foreground face) 30)))))
  (add-hook 'emacs-startup-hook 'rainbow-delimiters-using-stronger-colors))

;; Cursor
(use-package beacon
  :ensure t
  :custom
  (beacon-color "orange")
  :config
  (beacon-mode 1))


;;;;; Key Bindings ;;;;;
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)

(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(global-set-key (kbd "C-c C-n") 'rename-file-and-buffer)
(global-set-key (kbd "M-k") 'copy-line)

;;;;; Watch Python3 ;;;;;
(setq python-shell-interpreter "python3")


;;;;; corfu ;;;;;
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-quit-at-boundary nil)
  (corfu-scroll-margin 5)
  (corfu-echo-documentation t)
  (corfu-quit-no-match t)
  :bind
  (:map corfu-map
		("TAB" . corfu-insert)
		([tab] . corfu-insert)
		("C-n" . corfu-next)
		("C-p" . corfu-previous))
  :init
  (global-corfu-mode))


(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :init
  (corfu-popupinfo-mode)
  :config
  (setq corfu-popupinfo-delay 0.1))

(use-package corfu-quick
  :after corfu
  :ensure nil
  :bind
  (:map corfu-map
		("C-;" . corfu-quick-complete)))


;;;;; cape ;;;;;
(use-package cape
  :ensure t
  :config
  (defun my/lsp-capf ()
	(setq-local completion-at-point-functions
				(list (cape-capf-super
					   #'lsp-completion-at-point
					   (cape-company-to-capf #'company-yasnippet)))))
  (add-hook 'lsp-completion-mode-hook #'my/lsp-capf))


;;;;; kind-icon ;;;;;
(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;;;;; vertico ;;;;;
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  (setq vertico-count 20
		vertico-resize t
		vertico-cycle t))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
			  ("C-l" . vertico-directory-up)))

(use-package vertico-quick
  :after vertico
  :ensure nil
  :bind (:map vertico-map
			  ("C-;" . vertico-quick-jump)
			  ("C-'" . vertico-quick-exit)))


;;;;; Consult ;;;;;
(use-package consult
  :ensure t
  :bind
  (("M-y" . consult-yank-from-kill-ring)
   ("C-s" . consult-line)
   ("C-c s" . consult-line-multi)
   ("C-x b" . consult-buffer)
   ("C-c b" . consult-buffer-other-window)
   ("C-c l" . consult-goto-line)
   ("C-c f" . consult-find)
   ("C-c !" . consult-flymake))
  :config
  (use-package affe
	:ensure t))

(defun consult-ripgrep-symbol-at-point ()
  (interactive)
  (consult-ripgrep nil (thing-at-point 'symbol)))

(defun my-consult-ripgrep (use-symbol)
  (interactive "p")
  (cond ((eq use-symbol 1)
		 (call-interactively 'consult-ripgrep))
		((eq use-symbol 4)
		 (call-interactively 'consult-ripgrep-symbol-at-point))))
(global-set-key (kbd "C-c g") 'my-consult-ripgrep)


;;;;; orderless ;;;;;
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))


;;;;; marginalia ;;;;;
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))


;;;;; avy ;;;;;
(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "C-;") 'avy-goto-char-timer))


;;;;; git ;;;;;

;; magit
(use-package magit
  :ensure t
  :bind
  (("M-g" . magit-status)))

;; git-gutter
(use-package git-gutter
  :ensure t
  :custom
  (git-gutter:modified-sign "~")
  (git-gutter:added-sign    "+")
  (git-gutter:deleted-sign  "-")
  :custom-face
  (git-gutter:modified ((t (:background "#f1fa8c"))))
  (git-gutter:added    ((t (:background "#50fa7b"))))
  (git-gutter:deleted  ((t (:background "#ff79c6"))))
  :config
  (global-git-gutter-mode +1))

;; blamer
(use-package blamer
  :ensure t
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 40)
  (blamer-pretty-time-p t)
  (blamer-author-formatter "✎ %s ")
  (blamer-datetime-formatter "[%s] ")
  (blamer-commit-formatter "● %s")
  (blamer-type 'visual)
  :config
  (global-blamer-mode 1))

;; smerge
(use-package smerge-mode
  :diminish)


;;;;; flymake ;;;;;
(use-package flymake-diagnostic-at-point
  :ensure t
  :after flymake
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))


;;;; vterm ;;;;;
(use-package vterm
  :ensure t
  :defer t
  :init
  (defun open-shell-sub (new)
	(split-window-below)
	(enlarge-window 12)
	(other-window 1)
	(let ((term) (res))
	  (if (or new (null (setq term (dolist (buf (buffer-list) res)
									 (if (string-match "*terminal<[0-9]+>*" (buffer-name buf))
										 (setq res buf))))))
		  (vterm)
		(switch-to-buffer term))))
  (defun open-shell-sub-right (new)
	(split-window-right)
	(enlarge-window-horizontally 40)
	(other-window 1)
	(let ((term) (res))
	  (if (or new (null (setq term (dolist (buf (buffer-list) res)
									 (if (string-match "*terminal<[0-9]+>*" (buffer-name buf))
										 (setq res buf))))))
		  (vterm)
		(switch-to-buffer term))))
  (defun open-shell ()
	(interactive)
	(open-shell-sub t))
  (defun open-shell-r ()
	(interactive)
	(open-shell-sub-right t))
  (defun vterm-consult-yank-from-kill-ring-action (orig-fun &rest args)
	(if (equal major-mode 'vterm-mode)
		(let ((inhibit-read-only t)
			  (yank-undo-function (lambda (_start _end) (vterm-undo))))
		  (cl-letf (((symbol-function 'insert-for-yank)
					 (lambda (str) (vterm-send-string str t))))
			(apply orig-fun args)))
	  (apply orig-fun args)))
  :bind
  (("C-c m" . open-shell)
   ("C-c n" . open-shell-r))
  :config
  (setq vterm-always-compile-module t)
  (advice-add #'consult-yank-from-kill-ring :around #'vterm-consult-yank-from-kill-ring-action))


;; (use-package multi-vterm
;;   :ensure t)


;;;;; golang ;;;;;
(use-package go-mode
  :ensure t
  :defer t
  :mode ("\\.go$" . go-mode)
  :init
  (defun my/golangci-lint ()
	(interactive)
	(with-output-to-temp-buffer "*golangci-lint*"
	  (call-process-shell-command
	   (concat "cd " (vc-root-dir) "; golangci-lint run --color always") nil
	   "*golangci-lint*" t)
	  (pop-to-buffer "*golangci-lint*")
	  (ansi-color-apply-on-region 1 (buffer-size))))
  :config
  (setq gofmt-command "goimports")
  (lsp-register-custom-settings
   '(("gopls.hints"
	  ((assignVariableTypes . t)
	   (compositeLiteralFields . t)
	   (compositeLiteralTypes . t)
	   (constantValues . t)
	   (functionTypeParameters . t)
	   (parameterNames . t)
	   (rangeVariableTypes . t)))))
  (add-hook 'before-save-hook #'gofmt-before-save)
  (use-package gotest
	:after go-mode
	:ensure t
	:bind (:map go-mode-map
				("C-c x" . go-run)
				("C-c t c" . go-test-current-test)
				("C-c t f" . go-test-current-file)
				("C-c t a" . go-test-current-project))
	:config
	(setq go-test-args "-v -count=1")))


;;;;; rust ;;;;;
(use-package rust-mode
  :ensure t
  :defer t
  :mode ("\\.rs$" . rust-mode)
  :bind
  (:map rust-mode-map
		("C-c t c" . lsp-rust-analyzer-related-tests))
  :custom
  (lsp-inlay-hint-enable t)
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-rustc-source "discover")
  (lsp-rust-analyzer-linked-projects
   ["./Cargo.toml", "clippy_dev/Cargo.toml", "lintcheck/Cargo.toml"])
  :config
  (setq rust-format-on-save t))

(use-package cargo
  :ensure t
  :defer t
  :hook (rust-mode . cargo-minor-mode))


;;;;; kotlin ;;;;;
(use-package kotlin-mode
  :ensure t
  :defer t
  :mode ("\\.kt$" . kotlin-mode))

(use-package dap-kotlin
  :ensure nil)


;;;;; lisp ;;;;;
(setq inferior-lisp-program "clisp")
(add-to-list 'load-path (expand-file-name "~/.emacs.d/slime"))
(use-package slime
  :defer t
  :ensure t
  :config
  (slime-setup '(slime-repl slime-fancy slime-banner)))


;;;;; lua ;;;;;
(use-package lua-mode
  :defer t
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))


;;;;; web ;;;;;
(use-package web-mode
  :ensure t
  :defer t
  :mode ("\\.html?\\'" . web-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.astro\\$" . web-mode))
  (setq web-mode-enable-current-element-highlight t
		web-mode-enable-current-column-highlight t
		web-mode-enable-auto-pairing t))


;;;;; yaml ;;;;;
(use-package yaml-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))


;;;;; shell ;;;;;
;; (add-hook 'after-save-hook
;;           'executable-make-buffer-file-executable-if-script-p)


;;;;; markdown ;;;;;
(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("\\.md\\'" . gfm-mode)
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-header-scaling t)
  (markdown-indent-on-enter 'indent-and-new-item))


;;;;; Docker ;;;;;
(use-package docker
  :ensure t
  :bind (("C-c C-d" . docker)))

(use-package dockerfile-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package docker-compose-mode
  :ensure t
  :defer t)


;;;;; Custom Functions ;;;;;


;;;;; color ;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#000000" "#d54e53" "#b9ca4a" "#e7c547" "#7aa6da" "#c397d8" "#70c0b1" "#eaeaea"))
 '(dimmer-buffer-exclusion-regexps
   '("^\\*Minibuf-[0-9]+\\*" "^.\\*which-key\\*$" "^*Messages*" "*LV*" "transient" "*flycheck-posframe-buffer*") nil nil "Customized with use-package dimmer")
 '(fci-rule-color "#424242")
 '(frame-background-mode 'dark)
 '(jdee-db-active-breakpoint-face-colors (cons "#f0f0f0" "#4078f2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f0f0f0" "#50a14f"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f0f0f0" "#9ca0a4"))
 '(objed-cursor-color "#e45649")
 '(package-selected-packages
   '(nerd-icons dashboard dap-mode lsp-treemacs treemacs flymake-diagnostic-at-point highlight-indent-guides kotlin-mode java-mode multi-vterm c++-mode lsp-ui lsp-mode quelpa-use-package dired-subtree ace-window avy rust-mode rust cargo lua-mode multiple-cursors expand-region docker tree-sitter-langs tree-sitter dimmer blamer comment-dwim-2 corfu-doc kind-icon cape corfu eg exec-path-from-shell affe marginalia embark orderless consult vertico minimap yasnippet minions moody web-mode origami mwim presentation gotest which-key git-gutter hungry-delete vterm slime projectile go-mode beacon highlight-symbol dockerfile-mode docker-compose-mode yaml-mode toc-org aggressive-indent undo-tree hl-todo company rainbow-delimiters use-package helm-rtags company-lsp helm-config package-utils tide--cleanup-kinds disable-mouse auto-async-byte-compile helm-gtags magit cmake-ide color-theme-modern all-the-icons color-theme-sanityinc-tomorrow))
 '(pdf-view-midnight-colors (cons "#383a42" "#fafafa"))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#d54e53")
	 (40 . "#e78c45")
	 (60 . "#e7c547")
	 (80 . "#b9ca4a")
	 (100 . "#70c0b1")
	 (120 . "#7aa6da")
	 (140 . "#c397d8")
	 (160 . "#d54e53")
	 (180 . "#e78c45")
	 (200 . "#e7c547")
	 (220 . "#b9ca4a")
	 (240 . "#70c0b1")
	 (260 . "#7aa6da")
	 (280 . "#c397d8")
	 (300 . "#d54e53")
	 (320 . "#e78c45")
	 (340 . "#e7c547")
	 (360 . "#b9ca4a")))
 '(vc-annotate-very-old-color nil)
 '(window-divider-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:added ((t (:background "#50fa7b"))))
 '(git-gutter:deleted ((t (:background "#ff79c6"))))
 '(git-gutter:modified ((t (:background "#f1fa8c")))))

