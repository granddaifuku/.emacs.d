;; neotree
(require 'use-package)

(use-package neotree
  :ensure t
  :init
  (setq-default neo-keymap-style 'concise)
  :bind
  (("C-q" . neotree-toggle))
  :config
  (setq neo-smart-open t)
  (setq neo-create-file-auto-open t)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-show-hidden-files t)
  (setq neo-persist-show t))
