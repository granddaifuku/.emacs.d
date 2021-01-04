;; Added By Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; Package
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))

(package-initialize)

(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/elpa/use-package20200721.2156")
  (require 'use-package))
(use-package bind-key
  :ensure t)

;;;;; Encoding ;;;;;
(set-language-environment 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;;;; Environment ;;;;;
;; Auto Created FIles
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq delete-auto-save-files t)
(setq make-backup-files nil)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default cursor-type 'bar)
(setq default-directory "~/")
(setq command-line-default-directory "~/")
(setq inhibit-startup-message t)
(setq kill-whole-line t)
(savehist-mode 1)

(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode 1))

(use-package disable-mouse
  :ensure t
  :config
  (global-disable-mouse-mode))

(use-package hl-todo
  :ensure t
  :config
  (setq hl-todo-keyword-faces
		'(("TODO" . "#cc9393")
		  ("FIXME" . "#cc9393")
		  ("DEBUG" . "#A020F0")))
  (global-hl-todo-mode 1))


;;;;; auto-async-byte-compile ;;;;;

(use-package auto-async-byte-compile
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))


;;;;; Theme ;;;;;

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
		doom-themes-enable-italic t)
  (load-theme 'doom-snazzy t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config))


;;;;; Coding Style ;;;;;

;; line number
(global-linum-mode t)
(set-face-attribute 'linum nil
					:foreground "#a9a9a9"
					:height 0.9)
(setq linum-format "%4d ")

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


;;;;; Key Bindings ;;;;;

(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)

(defun copy-whole-line (&optional arg)
  "Copy current line."
  (interactive "p")
  (or arg (setq arg 1))
  (if (and (> arg 0) (eobp) (save-excursion (forward-visible-line 0) (eobp)))
	  (signal 'end-of-buffer nil))
  (if (and (< arg 0) (bobp) (save-excursion (end-of-visible-line) (bobp)))
	  (signal 'beginning-of-buffer nil))
  (unless (eq last-command 'copy-region-as-kill)
	(kill-new "")
	(setq last-command 'copy-region-as-kill))
  (cond ((zerop arg)
		 (save-excursion
		   (copy-region-as-kill (point) (progn (forward-visible-line 0) (point)))
		   (copy-region-as-kill (point) (progn (end-of-visible-line) (point)))))
		 ((< arg 0)
		  (save-excursion
			(copy-region-as-kill (point) (progn (end-of-visible-line) (point)))
			(copy-region-as-kill (point)
								 (progn (forward-visible-line (1+ arg))
										(unless (bobp) (backward-char))
										(point)))))
		(t
		 (save-excursion
		   (copy-region-as-kill (point) (progn (forward-visible-line 0) (point)))
		   (copy-region-as-kill (point)
								(progn (forward-visible-line arg) (point))))))
  (message (substring (car kill-ring-yank-pointer) 0 -1)))

(global-set-key (kbd "C-c C-n") 'rename-file-and-buffer)
(global-set-key (kbd "M-k") 'copy-whole-line)


;;;;; eglot ;;;;;

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd")))
  (add-to-list 'eglot-server-programs '(rustic-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pyls")))
  (add-to-list 'eglot-server-programs '(LaTeX-mode . ("digestif")))
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'rustic-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'LaTeX-mode-hook 'eglot-ensure)
  (define-key eglot-mode-map (kbd "C-c e f") 'eglot-format)
  (define-key eglot-mode-map (kbd "C-c e n") 'eglot-rename)
  )


;;;;; company ;;;;;

(use-package company
  :ensure t
  :bind
  (("C-M-i" . company-complete)
   :map company-active-map
   ("M-n" . nil)
   ("M-p" . nil)
   ("C-h" . nil)
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("C-s" . company-filter-candidates)
   ("C-i" . company-complete-selection)
   ([tab] . company-complete-selection))
  :init
  (global-company-mode)
  :config
  (setq company-backends '(company-capf))
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq completion-ignore-case t)
  (set-face-attribute 'company-tooltip nil
					  :foreground "#f5f5dc" :background "#696969")
  (set-face-attribute 'company-tooltip-common nil
					  :foreground "#f5f5dc" :background "#696969")
  (set-face-attribute 'company-tooltip-common-selection nil
					  :foreground "black" :background "steelblue")
  (set-face-attribute 'company-tooltip-selection nil
					  :foreground "white" :background "steelblue")
  (set-face-attribute 'company-preview-common nil
					  :background nil :foreground "lightgrey" :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
					  :background "orange")
  (set-face-attribute 'company-scrollbar-bg nil
					  :background "lightgrey"))


;;;;; flymake ;;;;;

(use-package flymake
  :init
  (defun flymake-cc-init ()
	(let* ((temp-file   (flymake-proc-init-create-temp-buffer-copy
						 'flymake-create-temp-inplace))
           (local-file  (file-relative-name
						 temp-file
						 (file-name-directory buffer-file-name))))
      (list "g++" (list "-Wall" "-Wextra" "-fsyntax-only" local-file))))
  :bind (
		 ("C-c ! p" . flymake-goto-next-error)
		 ("C-c ! n" . flymake-goto-prev-error))
  :config
  (push '("\\.cc$" flymake-cc-init) flymake-proc-allowed-file-name-masks)
  (push '("\\.cpp$" flymake-cc-init) flymake-proc-allowed-file-name-masks)
  (push '("\\.h$" flymake-cc-init) flymake-proc-allowed-file-name-masks)
  (push '("\\.hpp$" flymake-cc-init) flymake-proc-allowed-file-name-masks)
  (add-hook 'c++-mode-hook '(lambda () (flymake-mode t)))
  )

(use-package flymake-diagnostic-at-point
  :after flymake
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

;; ;;;;; flycheck ;;;;;

;; (use-package flycheck
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-hook 'rustic-mode-hook 'flycheck-mode)
;;   (add-hook 'c++-mode-hook 'flycheck-mode)
;;   :config
;;   (setq flycheck-check-syntax-automatically '(mode-enabled save)))

;; (use-package helm-flycheck
;;   :ensure t
;;   :after (flycheck)
;;   :bind(:map flycheck-mode-map
;; 			 ("C-c ! h" . helm-flycheck)))


;;;;; helm ;;;;;

(use-package helm
  :ensure t
  :bind(
		("C-x b" . helm-mini)
		("C-x C-f" . helm-find-files)
		("M-x" . helm-M-x)
		("C-c h" . helm-command-prefix)
		:map helm-map
		("<tab>" . helm-execute-persistent-action)
		("C-i" . helm-execute-persistent-action)
		("C-z" . helm-select-action)
		("C-x c" . nil))
  :init
  (defun spacemacs//helm-hide-minibuffer-maybe ()
	"Hide minibuffer in Helm session if we use the header line as input field."
	(when (with-helm-buffer helm-echo-input-in-header-line)
	  (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
		(overlay-put ov 'window (selected-window))
		(overlay-put ov 'face
					 (let ((bg-color (face-background 'default nil)))
					   `(:background ,bg-color :foreground ,bg-color)))
		(setq-local cursor-type nil))))
  :config
  (when (executable-find "curl")
	(defvar helm-google-suggest-use-curl-p t))
  (defvar helm-M-x-fuzzy-match t)
  (defvar helm-buffers-fuzzy-matching t)
  (defvar helm-recentf-fuzzy-match    t)
  (use-package helm-config
	:config
	(setq helm-split-window-inside-p           t
		  helm-move-to-line-cycle-in-source     t
		  helm-scroll-amount                    8
		  helm-echo-input-in-header-line t)
	(defvar helm-ff-search-library-in-sexp t)
	(defvar helm-ff-file-name-history-use-recentf t))
  (add-hook 'helm-minibuffer-set-up-hook
			'spacemacs//helm-hide-minibuffer-maybe)
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20)
  (helm-autoresize-mode 1)
  (helm-mode 1))


;;;;; magit ;;;;;

(use-package magit
  :ensure t
  :bind
  ("M-g" . magit-status))


;;;;; multi-term ;;;;;

(use-package multi-term
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
		  (multi-term)
		(switch-to-buffer term))))
  (defun open-shell-sub-right (new)
	(split-window-right)
	(enlarge-window-horizontally 40)
	(other-window 1)
	(let ((term) (res))
	  (if (or new (null (setq term (dolist (buf (buffer-list) res)
									 (if (string-match "*terminal<[0-9]+>*" (buffer-name buf))
										 (setq res buf))))))
		  (multi-term)
		(switch-to-buffer term))))
  (defun open-shell ()
	(interactive)
	(open-shell-sub t))
  (defun open-shell-r ()
	(interactive)
	(open-shell-sub-right t))
  (defun to-shell ()
	(interactive)
	(open-shell-sub nil))
  :bind
  ("C-c m" . open-shell)
  ("C-c n" . open-shell-r)
  (:map term-mode-map
		("C-c C-p" . multi-term-prev)
		("C-c C-n" . multi-term-next))
  :config
  (setq scroll-conservatively 1)
  (setq next-screen-context-lines 5)
  (setenv "SHELL" shell-file-name)
  (setq multi-term-program shell-file-name)
  (setq system-uses-terminfo t)
  (defadvice term-interrupt-subjob
	  (around ad-term-interrupt-subjob activate)
	(term-send-raw-string "\C-c")))


;;;;; yasnippet ;;;;;

(use-package yasnippet
  :ensure t
  :defer t
  :bind
  (("C-c y n" . yas-new-snippet)
   ("C-c y v" . yas-visit-snippet-file))
  :config
  (yas-global-mode 1))

(use-package helm-c-yasnippet
  :ensure t
  :defer t
  :bind
  (("C-c y i" . helm-yas-complete))
  :config
  (setq helm-yas-space-match-any-greedy t)
  (push '("emacs.+/snippets/" . snippet-mode) auto-mode-alist))


;;;;; rust ;;;;;

(use-package rustic
  :ensure t
  :defer t
  :mode ("\\.rs$" . rustic-mode)
  :config
  (setq rustic-lsp-client 'eglot)
  (setq-default rustic-format-trigger 'on-save)
  (setq rustic-rustfmt-bin "~/.cargo/bin/rustfmt")
  (add-to-list 'rustic-rustfmt-config-alist '("edition" . "2018")))


;;;;; typescript ;;;;;

(use-package typescript-mode
  :ensure t
  :defer t
  :mode
  (("\\.ts\\'" . typescript-mode)))

(use-package tide
  :ensure t
  :defer t
  :config
  (add-hook 'typescript-mode-hook
		  (lambda ()
			(tide-setup)
			(flycheck-mode t)
			(setq flycheck-check-syntax-automatically '(save mode-enabled))
			(eldoc-mode t)
			(company-mode-on))))


;;;;; markdown ;;;;;
(use-package markdown-preview-mode
  :ensure t
  :config
  ;; ToDo
  ;; Resolve the issue that dark style theme is not working.
  (setq markdown-preview-stylesheets (list "http://github.com/yrgoldteeth/darkdowncss/raw/master/darkdown.css"))
  (add-to-list 'markdown-preview-javascript "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML"))


;; ;;;;; tex ;;;;;
(use-package auctex
  :defer t
  :ensure t
  :config
  (setq TeX-default-mode 'japanese-latex-mode)
  (setq TeX-auto-save t
		TeX-parse-self t
		TeX-PDF-from-DVI "Dvipdfmx"
		preview-image-type 'dvipng)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook
			(lamgda ()
					(add-to-list 'TeX-command-list
								 '("pLaTeX" "%`%(PDF)platex %(file-line-error) %(extraopts) %S%(PDFout)%(mode)%' %T" TeX-run-TeX nil
								   (latex-mode doctex-mode)
								   :help "Run pLaTeX"))
					(add-to-list 'TeX-command-list
								 '("pBibTeX" "pbibtex %s" TeX-run-BibTeX nil
								   (plain-tex-mode latex-mode doctex-mode ams-tex-mode texinfo-mode context-mode)
								   :help "Run pBibTeX")))))

;;;;; neotree ;;;;;

(use-package neotree
  :ensure t
  :init
  (setq-default neo-keymap-style 'concise)
  :bind
  (("C-q" . neotree-toggle))
  :config
  (setq neo-smart-open t
		neo-create-file-auto-open t
		neo-theme (if (display-graphic-p) 'icons 'arrow)
		neo-show-hidden-files t)
  (defvar neo-persist-show t))



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
 '(beacon-color "#d54e53")
 '(custom-safe-themes
   '("4c8372c68b3eab14516b6ab8233de2f9e0ecac01aaa859e547f902d27310c0c3" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" default))
 '(fci-rule-color "#424242")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode 'dark)
 '(jdee-db-active-breakpoint-face-colors (cons "#f0f0f0" "#4078f2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f0f0f0" "#50a14f"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f0f0f0" "#9ca0a4"))
 '(objed-cursor-color "#e45649")
 '(package-selected-packages
   '(hl-todo auctex markdown-preview-mode flymake-diagnostic-at-point helm-company company eglot rainbow-delimiters tide neotree use-package doom-themes helm-lsp rustic helm-rtags company-lsp helm-config package-utils tide--cleanup-kinds typescript-mode helm-c-yasnippet disable-mouse smart-hungry-delete auto-async-byte-compile hungry-delete helm-gtags magit cmake-ide color-theme-modern all-the-icons multi-term color-theme-sanityinc-tomorrow helm))
 '(pdf-view-midnight-colors (cons "#383a42" "#fafafa"))
 '(rustic-ansi-faces
   ["#fafafa" "#e45649" "#50a14f" "#986801" "#4078f2" "#a626a4" "#0184bc" "#383a42"])
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
 '(helm-selection ((t (:extend t :background "dark cyan"))))
 '(helm-visible-mark ((t (:extend t :background "brightcyan" :foreground "black"))))
 '(mode-line ((t (:background "brown4"))))
 '(mode-line-inactive ((t (:background "gray30" :foreground "#e2e4e5"))))
 '(which-func ((t (:foreground "white")))))
