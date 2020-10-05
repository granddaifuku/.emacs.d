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
(require 'bind-key)

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))


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
(defvar linum-format "%4d ")

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


;;;;; flycheck ;;;;;

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode t)
  (setq flycheck-check-syntax-automatically '(mode-enabled save)))

(use-package helm-flycheck
  :ensure t
  :after (flycheck)
  :bind(:map flycheck-mode-map
			 ("C-c ! h" . helm-flycheck)))


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


;;;;; lsp ;;;;;

(use-package lsp-mode
  :ensure t
  :custom 
  ((lsp-print-io t)
   (lsp-trace t)
   (lsp-print-performance nil)
   ;;  (lsp-auto-guess-root t)
   (lsp-document-sync-method 'incremental)
   (lsp-response-timeout 5)
   (lsp-enable-completion-at-point t)
   (lsp-prefer-flymake nil))
  :hook
  ((rustic-mode c++-mode) . lsp)
  :config
  (setq gc-cons-threshold 100000000)
  (defvar lsp-completion-provider :capf)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-idle-delay 0.0))
 


(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :custom
  (lsp-ui-flycheck-enable t)
  ;; lsp-ui-doc
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-max-width 150)
  (lsp-ui-doc-max-height 30)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit t)
  ;; lsp-ui-sideline
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-symbol t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t)
  ;; lsp-ui-imenu
  (lsp-ui-imenu-enable nil)
  (lsp-ui-imenu-kind-position 'top)
  ;; lsp-ui-peek
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-peek-height 20)
  (lsp-ui-peek-list-width 50)
  (lsp-ui-peek-fontify 'on-demand)
  ;; :preface
  ;; (defun ladicle/toggle-lsp-ui-doc ()
  ;;   (interactive)
  ;;   (if lsp-ui-doc-mode
  ;;       (progn
  ;;         (lsp-ui-doc-mode -1)
  ;;         (lsp-ui-doc-hide-frame))
  ;;     (lsp-ui-doc-mode 1)))
  :bind
  (:map lsp-mode-map
		("C-c C-r" . lsp-ui-peek-find-references)
		("C-c C-j" . lsp-ui-peek-find-definitions)
		("C-c i"   . lsp-ui-peek-find-implementation)
		("C-c C-m" . lsp-ui-imenu)
		("C-c s"   . lsp-ui-sideline-mode)
		("C-c d"   . ladicle/toggle-lsp-ui-doc))
  :hook
  (lsp-mode . lsp-ui-mode))


(use-package ccls
  :ensure t
  :after lsp-mode
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp)))
  :config
  (setq ccls-initialization-options `(:clang (:extraArgs ["--gcc-toolchain=/usr"])))
  (setq ccls-executable "/usr/local/opt/ccls/build/Release/ccls")
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  (setq ccls-args '("--log-file=/tmp/ccls.log")))


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



;;;;; Python ;;;;;

(use-package elpy
  :ensure t
  :defer t
  :config
  (elpy-enable)
  (when (load "flycheck" t t)
	(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
	(add-hook 'elpy-mode-hook 'flycheck-mode)))


;;;;; rust ;;;;;

(use-package rustic
  :ensure t
  :defer t
  ;; :init
  ;; (add-hook 'rustic-mode-hook 'racer-mode)
  :mode ("\\.rs$" . rustic-mode))
  ;; :config
  ;; (setq lsp-rust-analyzer-server-command '("~/.cargo/bin/rust-analyzer")))
  ;; (use-package flycheck-rust
  ;; 	:ensure t)
  ;; (use-package quickrun
  ;; 	:ensure t)
  ;; (use-package racer
  ;; 	:ensure t))
  ;; (setq rustic-format-trigger nil))


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
;; '(custom-enabled-themes '(clarity))
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
   '(lsp-mode rainbow-delimiters tide neotree use-package doom-themes helm-lsp lsp-ui racer rustic flycheck-pkg-config helm-rtags ccls company-lsp helm-config package-utils tide--cleanup-kinds helm-flycheck typescript-mode helm-c-yasnippet disable-mouse smart-hungry-delete auto-async-byte-compile hungry-delete helm-gtags magit elpy cmake-ide flycheck-irony color-theme-modern all-the-icons multi-term flycheck color-theme-sanityinc-tomorrow helm))
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
 '(which-func ((t (:foreground "white")))))
