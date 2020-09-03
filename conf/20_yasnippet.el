;; yasnippet
(require 'use-package)

(use-package yasnippet
  :ensure t
  :defer t
  :bind
  (("C-c y n" . yas-new-snippet)
   ("C-c y v" . yas-visit-snippet-file))
  :config
  (yas-global-mode 1))

(use-package helm-c-yasnippet
  :ensure t
  :defer t
  :bind
  (("C-c y i" . helm-yas-complete))
  :config
  (setq helm-yas-space-match-any-greedy t)
  (push '("emacs.+/snippets/" . snippet-mode) auto-mode-alist))

;; (require 'yasnippet)
;; (require 'helm-c-yasnippet)
;; (setq helm-yas-space-match-any-greedy t)
;; (global-set-key (kbd "C-c y") 'helm-yas-complete)
;; (push '("emacs.+/snippets/" . snippet-mode) auto-mode-alist)
;; (yas-global-mode 1)
;; (yas-load-directory "~/.emacs.d/yasnippet/")
