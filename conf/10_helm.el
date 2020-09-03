;; helm
(require 'use-package)

(use-package helm
  :ensure t
  :bind(
		("C-x b" . helm-mini)
		("C-x C-f" . helm-find-files)
		("M-x" . helm-M-x)
		("C-c h" . helm-command-prefix)
		:map helm-map
		("<tab>" . helm-execute-persistent-action)
		("C-i" . helm-execute-persistent-action)
		("C-z" . helm-select-action)
		("C-x c" . nil))
  :init
  (defun spacemacs//helm-hide-minibuffer-maybe ()
	"Hide minibuffer in Helm session if we use the header line as input field."
	(when (with-helm-buffer helm-echo-input-in-header-line)
	  (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
		(overlay-put ov 'window (selected-window))
		(overlay-put ov 'face
					 (let ((bg-color (face-background 'default nil)))
					   `(:background ,bg-color :foreground ,bg-color)))
		(setq-local cursor-type nil))))
  :config
  (when (executable-find "curl")
	(setq helm-google-suggest-use-curl-p t))
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t
		helm-recentf-fuzzy-match    t)
  (use-package helm-config
	:config
	(setq helm-split-window-in-side-p           t
		  helm-move-to-line-cycle-in-source     t
		  helm-ff-search-library-in-sexp        t
		  helm-scroll-amount                    8
		  helm-ff-file-name-history-use-recentf t
		  helm-echo-input-in-header-line t))
  (add-hook 'helm-minibuffer-set-up-hook
			'spacemacs//helm-hide-minibuffer-maybe)
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20)
  (helm-autoresize-mode 1)
  (helm-mode 1)
  )

;; (require 'helm)
;; (require 'helm-config)
;; (require 'helm-gtags)


;; (global-set-key (kbd "C-x b") 'helm-mini) 
;; (global-set-key (kbd "C-c h") 'helm-command-prefix)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-unset-key (kbd "C-x c"))

;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
;; (define-key helm-map (kbd "C-z") 'helm-select-action)

;; (when (executable-find "curl")
;;   (setq helm-google-suggest-use-curl-p t))

;; (setq helm-split-window-in-side-p           t
;; 	  helm-move-to-line-cycle-in-source     t
;; 	  helm-ff-search-library-in-sexp        t
;; 	  helm-scroll-amount                    8
;; 	  helm-ff-file-name-history-use-recentf t
;; 	  helm-echo-input-in-header-line t)

;; (defun spacemacs//helm-hide-minibuffer-maybe ()
;;   "Hide minibuffer in Helm session if we use the header line as input field."
;;   (when (with-helm-buffer helm-echo-input-in-header-line)
;; 	(let ((ov (make-overlay (point-min) (point-max) nil nil t)))
;; 	  (overlay-put ov 'window (selected-window))
;; 	  (overlay-put ov 'face
;; 				   (let ((bg-color (face-background 'default nil)))
;; 					 `(:background ,bg-color :foreground ,bg-color)))
;; 	  (setq-local cursor-type nil))))

;; (add-hook 'helm-minibuffer-set-up-hook
;; 		  'spacemacs//helm-hide-minibuffer-maybe)

;; (setq helm-autoresize-max-height 0)
;; (setq helm-autoresize-min-height 20)
;; (setq helm-M-x-fuzzy-match t)
;; (setq helm-buffers-fuzzy-matching t
;; 	  helm-recentf-fuzzy-match    t)
;; (helm-autoresize-mode 1)
;; (helm-mode 1)



;; helm-gtags
;; (helm-gtags-mode t)
;; (setq helm-gtags-auto-update t)
;; (setq helm-gtags-ignore-case t)
;; (setq helm-gtags-mode-hook
;;   '(lambda ()
;;   (local-unset-key "\C-t")
;;   ; 文脈から判断してジャンプ
;;   (local-set-key "\C-t\C-t" 'helm-gtags-dwim)
;;   ; 定義元へ
;;   (local-set-key "\C-t\C-d" 'helm-gtags-find-tag)
;;   ; 参照元へ
;;   (local-set-key "\C-t\C-r" 'helm-gtags-find-rtag)
;;   ; 変数の定義元/参照先へ
;;   (local-set-key "\C-t\C-s" 'helm-gtags-find-symbol)
;;   ; 前のバッファへ
;;   (local-set-key "\C-t\C-p" 'helm-gtags-previous-history)
;;   ; 次のバッファへ
;;   (local-set-key "\C-t\C-n" 'helm-gtags-next-history)
;;   ; ファイルへ
;;   (local-set-key "\C-t\C-f" 'helm-gtags-find-file)
;;   ))
