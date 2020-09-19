;; flycheck
(require 'use-package)

(use-package flycheck
  :ensure t
  :defer t
  :config
  (global-flycheck-mode t)
  (setq flycheck-check-syntax-automatically '(mode-enabled save)))


;; (use-package rtags
;;   :ensure t
;;   :defer t)

(use-package helm-flycheck
  :ensure t
  :defer t
  :after (flycheck)
  :bind(:map flycheck-mode-map
			 ("C-c ! h" . helm-flycheck)))
