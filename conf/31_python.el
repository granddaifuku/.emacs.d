;; Elpy
(require 'use-package)
(use-package elpy
  :ensure t
  :defer t
  :config
  (elpy-enable)
  (when (load "flycheck" t t)
	(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
	(add-hook 'elpy-mode-hook 'flycheck-mode)))
