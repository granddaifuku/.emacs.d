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

;;;;; auto-async-byte-compile ;;;;;
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
;; Auto Created Files
(setq auto-save-default nil)
(setq create-lockfiles nil)
(setq delete-auto-save-files t)
(setq make-backup-files nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-popup-type 'minibuffer))

;; window size
(toggle-frame-maximized)

;; presentation
(use-package presentation
  :ensure t)

;; smart move
(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default cursor-type 'bar)
(setq default-directory "~/")
(setq command-line-default-directory "~/")
(setq inhibit-startup-message t)
(setq kill-whole-line t)
(savehist-mode 1)

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
  :config
  (global-disable-mouse-mode))

;; highlight keyword
(use-package hl-todo
  :ensure t
  :custom
  (setq hl-todo-keyword-faces
		'(("TODO" . "#cc9393")
		  ("FIXME" . "#cc9393")
		  ("DEBUG" . "#A020F0")))
  :config
  (global-hl-todo-mode 1))

;; hightlight symbol
(use-package highlight-symbol
  :ensure t
  :config
  (setq highlight-symbol-idle-delay 0.5)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode) 
  (add-hook 'prog-mode-hook 'highlight-symbol-nav-mode)
  (add-hook 'LaTeX-mode-hook 'highlight-symbol-mode)
  (add-hook 'LaTeX-mode-hook 'highlight-symbol-nav-mode))


;;;;; Undo Tree ;;;;;
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history nil))


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
		neo-show-hidden-files t))


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
  (add-hook 'prog-mode-hook
			'(lambda ()
			   (flyspell-prog-mode)))
  (add-hook 'LaTeX-mode-hook
			'(lambda ()
			   (flyspell-mode))))


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
	  modus-themes-mode-line '(moody (padding . 6) (height . 1.5))
	  modus-themes-prompts '(bold background))
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


;;;;; Org mode ;;;;;
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(use-package toc-org
  :ensure t
  :init
  (add-hook 'org-mode-hook 'toc-org-mode)
  ;; (add-hook 'markdown-mode-hook 'toc-org-mode)
  ;; :config
  ;; (define-key markdown-mode-map (kbd "C-c C-o") 'toc-org-markdown-follow-thing-at-point)
  )

(use-package ox-hugo
  :ensure t
  :after ox
  :config
  (setq org-hugo-auto-set-lastmod t))

;; org capture
(with-eval-after-load 'org-capture
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: ")) ;; Prompt to enter the post title
		   (fname (read-from-minibuffer "Post URL: "))
		   ;; (fname (org-hugo-slug title))
		   (date (format-time-string "%Y-%m-%d" (org-current-time)))
		   (section (format-time-string "%Y/%m" (org-current-time))))
	  (mapconcat #'identity
                 `(
				   ,(concat "* TODO " title)
				   ":PROPERTIES:"
				   "EXPORT_HUGO_FRONT_MATTER_FORMAT: yaml"
				   ,(concat ":EXPORT_FILE_NAME: " fname)
				   ,(concat ":EXPORT_DATE: " date)
				   ,(concat ":EXPORT_HUGO_LASTMOD: " date)
				   ,(concat ":EXPORT_HUGO_SECTION*: " section)
				   ,(concat ":EXPORT_HUGO_CUSTOM_FRONT_MATTER: :thumbnail \"images/\"" )
				   ,(concat ":EXPORT_HUGO_CUSTOM_FRONT_MATTER+: :description \"\"")
				   ,(concat ":EXPORT_HUGO_CUSTOM_FRONT_MATTER+: :math true")
				   ":END:"
				   "#+BEGIN_SRC yaml :front_matter_extra t"
				   "menu:"
				   "  sidebar:"
				   "    name: "
				   "    identifier: "
				   ,(concat "    parent: " (downcase section))
				   "    weight: 10"
				   "#+END_SRC"
				   "%?\n")          ;; Place the cursor here finally
				 "\n")))
  (add-to-list 'org-capture-templates
			   '("j"                ;`org-capture' binding + j
                 "Hugo Japangese post"
                 entry
                 ;; It is assumed that below file is present in `org-directory'
                 ;; and that it has a "Blog Ideas" heading. It can even be a
                 ;; symlink pointing to the actual location of blog.org!
                 (file+olp "blog_ja.org" "Blog Ideas")
                 (function org-hugo-new-subtree-post-capture-template)))
  (add-to-list 'org-capture-templates
			   '("e"                ;`org-capture' binding + e
                 "Hugo English post"
                 entry
                 ;; It is assumed that below file is present in `org-directory'
                 ;; and that it has a "Blog Ideas" heading. It can even be a
                 ;; symlink pointing to the actual location of blog.org!
                 (file+olp "blog_en.org" "Blog Ideas")
                 (function org-hugo-new-subtree-post-capture-template))))


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
  ("C-c C-m" . minimap-mode))

;; Code folding
(use-package origami
  :ensure t
  :diminish
  :hook (after-init . global-origami-mode)
  :bind
  ("C-c o o" . origami-open-node)
  ("C-c o c" . origami-close-node))

;; line number
(global-display-line-numbers-mode)

;; indentation
(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode))

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
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(global-set-key (kbd "C-c C-n") 'rename-file-and-buffer)
(global-set-key (kbd "M-k") 'copy-line)

;;;;; Watch Python3 ;;;;;
(setq python-shell-interpreter "python3")


;;;;; company ;;;;;
(use-package company
  :ensure t
  :diminish company-mode
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
  :hook
  (after-init . global-company-mode)
  :config
  (setq company-backends '((company-capf :with company-yasnippet)))
  (setq company-idle-delay 0
		company-minimum-prefix-length 2
		company-selection-wrap-around t
		completion-ignore-case t
		company-show-quick-access t))


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
  :ensure t
  :after flymake
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))


;;;;; vertico ;;;;;
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  (setq vertico-count 20
		vertico-resize t
		vertico-cycle t))

(use-package consult
  :ensure t
  :bind
  ("M-y" . consult-yank-from-kill-ring)
  ("C-s" . consult-line)
  ("C-x b" . consult-buffer)
  ("C-c b" . consult-buffer-other-window)
  ("C-c l" . consult-goto-line)
  ("C-c g" . consult-grep)
  :config
  (use-package affe
	:ensure t))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))


;;;;; git ;;;;;

;; magit
(use-package magit
  :ensure t
  :bind
  ("M-g" . magit-status))

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

;; smerge
(use-package smerge-mode
  :diminish)


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
  :bind
  ("C-c m" . open-shell)
  ("C-c n" . open-shell-r)
  :config
  (setq vterm-always-compile-module t))


;;;;; yasnippet ;;;;;
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :bind
  (("C-c y n" . yas-new-snippet)
   ("C-c y v" . yas-visit-snippet-file)
   ("C-c y i" . yas-insert-snippet))
  ;; ([tab] . yas-expand))
  :config
  (yas-global-mode 1))


;;;;; eglot ;;;;;
(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '(c-mode . ("clangd")))
  (add-to-list 'eglot-server-programs '(c++-mode . ("clangd")))
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls")))
  (add-to-list 'eglot-server-programs '(rustic-mode . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pyls")))
  (add-to-list 'eglot-server-programs '(LaTeX-mode . ("digestif")))
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'rustic-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'LaTeX-mode-hook 'eglot-ensure)
  ;; format on save
  (add-hook 'c-mode-hook '(lambda() (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
  (add-hook 'c++-mode-hook '(lambda() (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
  (add-hook 'python-mode-hook '(lambda() (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename))


;;;;; golang ;;;;;
(use-package go-mode
  :ensure t
  :defer t
  :mode ("\\.go$" . go-mode)
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook #'gofmt-before-save)
  (use-package gotest
	:after go-mode
	:ensure t
	:bind (:map go-mode-map
				("C-c x" . go-run)
				("C-c t" . go-test-current-test)
				("C-c f" . go-test-current-file)
				("C-c a" . go-test-current-project))
	:config
	(setq go-test-args "-v -count=1")))


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


;;;;; lisp ;;;;;
(setq inferior-lisp-program "clisp")
(add-to-list 'load-path (expand-file-name "~/.emacs.d/slime"))
(use-package slime
  :ensure t
  :config
  (slime-setup '(slime-repl slime-fancy slime-banner)))


;;;;; web ;;;;;
(use-package web-mode
  :ensure t
  :defer t
  :mode ("\\.html?\\'" . web-mode)
  :config
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
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package markdown-preview-mode
  :ensure t
  :config
  ;; TODO
  ;; Resolve the issue that dark style theme is not working.
  (setq markdown-preview-stylesheets (list "http://github.com/yrgoldteeth/darkdowncss/raw/master/darkdown.css"))
  (add-to-list 'markdown-preview-javascript "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML"))


;;;;; tex ;;;;;
(use-package auctex
  :defer t
  :ensure t
  :config
  (setq TeX-default-mode 'japanese-latex-mode
		TeX-auto-save t
		TeX-parse-self t
		TeX-PDF-from-DVI "Dvipdfmx"
		preview-image-type 'dvipng)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook
			(lambda ()
			  (add-to-list 'TeX-command-list
						   '("pLaTeX" "%`%(PDF)platex %(file-line-error) %(extraopts) %S%(PDFout)%(mode)%' %T" TeX-run-TeX nil
							 (latex-mode doctex-mode)
							 :help "Run pLaTeX"))
			  (add-to-list 'TeX-command-list
						   '("pBibTeX" "pbibtex %s" TeX-run-BibTeX nil
							 (plain-tex-mode latex-mode doctex-mode ams-tex-mode texinfo-mode context-mode)
							 :help "Run pBibTeX")))))


;;;;; Docker ;;;;;
(use-package dockerfile-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))
(use-package docker-compose-mode
  :ensure t)


;;;;; Custom Functions ;;;;;
;; Format Json (requires jq)
(defun jq-format (beg end)
  (interactive "r")
  (shell-command-on-region beg end "jq . " nil t))


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
 '(fci-rule-color "#424242")
 '(frame-background-mode 'dark)
 '(jdee-db-active-breakpoint-face-colors (cons "#f0f0f0" "#4078f2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f0f0f0" "#50a14f"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f0f0f0" "#9ca0a4"))
 '(objed-cursor-color "#e45649")
 '(package-selected-packages
   '(eg exec-path-from-shell affe marginalia embark orderless consult vertico minimap yasnippet minions moody web-mode origami mwim presentation gotest which-key git-gutter hungry-delete vterm slime projectile go-mode beacon ox-hugo highlight-symbol dockerfile-mode docker-compose-mode yaml-mode toc-org aggressive-indent undo-tree hl-todo auctex markdown-preview-mode flymake-diagnostic-at-point company eglot rainbow-delimiters neotree use-package rustic helm-rtags company-lsp helm-config package-utils tide--cleanup-kinds disable-mouse auto-async-byte-compile helm-gtags magit cmake-ide color-theme-modern all-the-icons color-theme-sanityinc-tomorrow))
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
 '(git-gutter:added ((t (:background "#50fa7b"))))
 '(git-gutter:deleted ((t (:background "#ff79c6"))))
 '(git-gutter:modified ((t (:background "#f1fa8c")))))
