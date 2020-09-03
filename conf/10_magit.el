;; magit
(require 'use-package)
(use-package magit
  :ensure t
  :bind
  ("M-g" . magit-status))
;;(require 'magit)
;;(define-key global-map (kbd "M-g") 'magit-status)

