;; irony
(require 'use-package)

;; (require 'flymake)

;; (defun flymake-cc-init ()
;;   (let* ((temp-file   (flymake-proc-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;          (local-file  (file-relative-name
;;                        temp-file
;;                        (file-name-directory buffer-file-name))))
;;     (list "g++" (list "-Wall" "-Wextra" "-fsyntax-only" local-file))))

;; (push '("\\.cpp$" flymake-cc-init) flymake-proc-allowed-file-name-masks)

;; (add-hook 'c++-mode-hook
;;           '(lambda ()
;;              (flymake-mode t)))
;; (remove-hook 'c++-mode-hook
;;           '(lambda ()
;;              (flycheck-mode t)))


;; (use-package cmake-ide
;;   :ensure t
;;   :defer t)

;; (use-package rtags
;;   :ensure t
;;   :defer t
;;   :config
;;   (progn
;; 	(rtags-enable-standard-keybindings c-mode-base-map)
;; 	(cmake-ide-setup))
;;   (setq rtags-completions-enabled t))





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
