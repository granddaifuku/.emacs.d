;; company
(require 'use-package)

(use-package company
  :ensure t
  :bind
  (("C-M-i" . company-complete)
   :map company-active-map
   ("M-n" . nil)
   ("M-p" . nil)
   ("C-h" . nil)
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("C-s" . company-filter-candidates)
   ("C-i" . company-complete-selection)
   ([tab] . company-complete-selection))
  :init
  (global-company-mode)
  :config
  (setq company-backends '(company-capf))
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq completion-ignore-case t)
  (set-face-attribute 'company-tooltip nil
					  :foreground "#f5f5dc" :background "#696969")
  (set-face-attribute 'company-tooltip-common nil
					  :foreground "#f5f5dc" :background "#696969")
  (set-face-attribute 'company-tooltip-common-selection nil
					  :foreground "black" :background "steelblue")
  (set-face-attribute 'company-tooltip-selection nil
					  :foreground "white" :background "steelblue")
  (set-face-attribute 'company-preview-common nil
					  :background nil :foreground "lightgrey" :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
					  :background "orange")
  (set-face-attribute 'company-scrollbar-bg nil
					  :background "lightgrey"))
