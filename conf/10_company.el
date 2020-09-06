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
					  :foreground "#white" :background "steelblue")
  (set-face-attribute 'company-preview-common nil
					  :background nil :foreground "lightgrey" :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
					  :background "orange")
  (set-face-attribute 'company-scrollbar-bg nil
					  :background "lightgrey"))

;; (require 'company)
;; (global-company-mode)
;; (setq company-idle-delay 0)
;; (setq company-minimum-prefix-length 2)
;; (setq company-selection-wrap-around t)
;; (setq completion-ignore-case t)

;; (global-set-key (kbd "C-M-i") 'company-complete)
;; (define-key company-active-map (kbd "M-n") nil)
;; (define-key company-active-map (kbd "M-p") nil)
;; (define-key company-active-map (kbd "C-n") 'company-select-next)
;; (define-key company-active-map (kbd "C-p") 'company-select-previous)
;; (define-key company-active-map (kbd "C-h") nil)
;; (define-key company-active-map (kbd "C-s") 'company-filter-candidates)
;; (define-key company-active-map (kbd "C-i") 'company-complete-selection)
;; (define-key company-active-map [tab] 'company-complete-selection)

;; color
;; (set-face-attribute 'company-tooltip nil
;; 					:foreground "#f5f5dc" :background "#696969")
;; (set-face-attribute 'company-tooltip-common nil
;; 					:foreground "#f5f5dc" :background "#696969")
;; (set-face-attribute 'company-tooltip-common-selection nil
;; 					:foreground "black" :background "steelblue")
;; (set-face-attribute 'company-tooltip-selection nil
;; 					:foreground "#white" :background "steelblue")
;; (set-face-attribute 'company-preview-common nil
;; 					:background nil :foreground "lightgrey" :underline t)
;; (set-face-attribute 'company-scrollbar-fg nil
;; 					:background "orange")
;; (set-face-attribute 'company-scrollbar-bg nil
;; 					:background "lightgrey")
