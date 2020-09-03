;; irony
(require 'use-package)

(use-package irony
  :ensure t
  :defer t
  :init
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'objc-mode 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-irony
  :ensure t
  :defer t
  :config
  (add-to-list 'company-backends 'company-irony))

;; (require 'irony)
;; (add-hook 'c-mode-hook 'irony-mode)
;; (add-hook 'c++-mode-hook 'irony-mode)
;; (add-hook 'objc-mode 'irony-mode)
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-irony))
