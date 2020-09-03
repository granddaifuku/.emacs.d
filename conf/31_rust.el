;; rust
(require 'use-package)

(add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
(use-package rustic
			 :ensure t
			 :defer t
			 :init
			 (add-hook 'rustic-mode-hook
					   '(lambda ()
						  (racer-mode t)
						  (dumb-jump-mode t)
						  (highlight-symbol-mode t)
						  (rainbow-delimiters-mode t)
						  (smartparens-mode t)))
			 :mode ("\\.rs$" . rustic-mode)
			 :commands (rustic-mode)
			 :config
			 (use-package quickrun
			   :ensure t
			   :defer t)
		     (use-package racer
			   :ensure t
			   :defer t)
			 (use-package lsp-mode
			   :ensure t)
			 (setq rustic-format-trigger nil))
