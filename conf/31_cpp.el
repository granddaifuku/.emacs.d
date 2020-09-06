;; irony
(require 'use-package)

(use-package cmake-ide
  :ensure t)

(use-package rtags
  :ensure t
  :after (cmake-ide)
  :config
  (cmake-ide-setup))

;; (use-package irony
;;   :ensure t
;;   :init
;;   (add-hook 'c-mode-hook 'irony-mode)
;;   (add-hook 'c++-mode-hook 'irony-mode)
;;   (add-hook 'objc-mode 'irony-mode)
;;   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;;   (setq irony-lang-compile-option-alist
;; 		(quote ((c++-mode . "c++ -lstdc++")
;; 				(c-mode . "c")
;; 				(objc-mode . "objective-c")))))

;; (use-package company-irony
;;   :ensure t
;;   :after (company)
;;   :config
;;   (add-to-list 'company-backends 'company-irony))

;; (require 'irony)
;; (add-hook 'c-mode-hook 'irony-mode)
;; (add-hook 'c++-mode-hook 'irony-mode)
;; (add-hook 'objc-mode 'irony-mode)
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-irony))
