;; rust
(require 'use-package)


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
