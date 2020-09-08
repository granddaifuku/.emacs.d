;; lsp
(require 'use-package)

(use-package lsp-mode
  :ensure t
  :custom
  (lsp-print-io nil)
  (lsp-trace nil)
  (lsp-print-performance nil)
  (lsp-auto-guess-root t)
  (lsp-document-sync-method 'incremental)
  (lsp-response-timeout 5)
  (lsp-enable-completion-at-point nil)
  (lsp-prefer-flymake nil)
  (lsp-prefer-capf t)
  :config
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc)))

;; (use-package company-lsp
;;   :after (lsp-mode company)
;;   :ensure t
;;   :init
;;   (push 'company-lsp company-backends)
;;   :custom
;;   (company-lsp-cache-candidates t)
;;   (company-lst-asnyc t)
;;   (company-lsp-enable-recompletion nil))

(use-package lsp-ui
  :ensure t
  :custom
  ;; lsp-ui-doc
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-max-width 150)
  (lsp-ui-doc-max-height 30)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit t)
  ;; lsp-ui-sideline
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-symbol t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-sideline-show-code-actions nil)
  ;; lsp-ui-imenu
  (lsp-ui-imenu-enable nil)
  (lsp-ui-imenu-kind-position 'top)
  ;; lsp-ui-peek
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-peek-height 20)
  (lsp-ui-peek-list-width 50)
  (lsp-ui-peek-fontify 'on-demand)
  :preface
  (defun ladicle/toggle-lsp-ui-doc ()
    (interactive)
    (if lsp-ui-doc-mode
        (progn
          (lsp-ui-doc-mode -1)
          (lsp-ui-doc--hide-frame))
      (lsp-ui-doc-mode 1)))
  :bind
  (:map lsp-mode-map
		("C-c C-r" . lsp-ui-peek-find-references)
		("C-c C-j" . lsp-ui-peek-find-definitions)
		("C-c i"   . lsp-ui-peek-find-implementation)
		("C-c C-m"   . lsp-ui-imenu)
		("C-c s"   . lsp-ui-sideline-mode)
		("C-c d"   . ladicle/toggle-lsp-ui-doc))
  :hook
  (lsp-mode . lsp-ui-mode))

(use-package ccls
  :ensure t
  :after (lsp-mode)
  :custom
  (ccls-executable "/usr/local/Cellar/ccls/0.20190823.6/bin/ccls")
  (lsp-prefer-flymake nil)
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp)))
  :config
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc)))
