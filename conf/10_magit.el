;; magit

(require 'use-package)
(use-package magit
  :ensure t
  :bind
  ("M-g" . magit-status))
