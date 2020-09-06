;; flycheck
(require 'use-package)

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode t)
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  '(progn
	 (when (locate-library "flycheck-irony")
	   (flycheck-irony-setup))))

;; (use-package rtags
;;   :ensure t
;;   :defer t)

(use-package helm-flycheck
  :ensure t
  :defer t
  :after (flycheck)
  :bind(:map flycheck-mode-map
			 ("C-c ! h" . helm-flycheck)))

;; (require 'flycheck)
;; (global-flycheck-mode t)
;; (eval-after-load "flycheck"
;;   '(progn
;; 	 (when (locate-library "flycheck-irony")
;; 	   (flycheck-irony-setup))))
;; (setq flycheck-check-syntax-automatically '(mode-enabled save))


;; rtags
;; (when (require 'rtags nil 'noerror)
;;   (add-hook 'c-mode-common-hook
;;             (lambda ()
;;               (when (rtags-is-indexed)
;;                 (local-set-key (kbd "M-.") 'rtags-find-symbol-at-point)
;;                 (local-set-key (kbd "M-;") 'rtags-find-symbol)
;;                 (local-set-key (kbd "M-@") 'rtags-find-references)
;;                 (local-set-key (kbd "M-,") 'rtags-location-stack-back)))))

;; (require 'helm-flycheck)
;; (eval-after-load 'flycheck
;;   '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))
