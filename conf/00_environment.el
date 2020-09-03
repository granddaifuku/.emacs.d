;; Environment

(require 'use-package)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default cursor-type 'bar)
(setq default-directory "~/")
(setq command-line-default-directory "~/")
(setq inhibit-startup-message t)
(savehist-mode 1)
(global-hungry-delete-mode 1)

(use-package disable-mouse
  :ensure t
  :config
  (global-disable-mouse-mode))

;;(require 'disable-mouse)
