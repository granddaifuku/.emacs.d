;; rust
(require 'use-package)


;; (add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
(use-package rustic
  :ensure t
  :defer t
  ;; :init
  ;; (add-hook 'rustic-mode-hook 'racer-mode)
  :mode ("\\.rs$" . rustic-mode))
  ;; :config
  ;; (use-package flycheck-rust
  ;; 	:ensure t)
  ;; (use-package quickrun
  ;; 	:ensure t)
  ;; (use-package racer
  ;; 	:ensure t)
  ;; (setq rustic-format-trigger nil))
