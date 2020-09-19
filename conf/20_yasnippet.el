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
