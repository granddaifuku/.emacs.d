;; typescript
(require 'use-package)

(use-package typescript-mode
  :ensure t
  :defer t
  :mode
  (("\\.ts\\'" . typescript-mode)))

;; (require 'typescript-mode)
;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

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

;; (require 'tide)
;; (add-hook 'typescript-mode-hook
;; 		  (lambda ()
;; 			(tide-setup)
;; 			(flycheck-mode t)
;; 			(setq flycheck-check-syntax-automatically '(save mode-enabled))
;; 			(eldoc-mode t)
;; 			(company-mode-on)))
